// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Span, Spanned,
};
use hashbrown::{hash_map, HashMap, HashSet};
use rust_decimal::Decimal;
use std::{
    fmt::Display,
    io::{self, Write},
    path::Path,
};
use steel::{
    rvals::Custom,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use steel_derive::Steel;

#[derive(Clone, Debug, Steel)]
pub struct Ledger {
    sources: BeancountSources,
    accounts: HashMap<String, Account>,
}

impl Ledger {
    pub fn parse_from<W>(path: &Path, error_w: W) -> Result<Ledger, Error>
    where
        W: Write + Copy,
    {
        let sources = BeancountSources::try_from(path).map_err(Error::Io)?;
        let parser = BeancountParser::new(&sources);

        let builder = match parser.parse() {
            Ok(ParseSuccess {
                directives,
                options: _,
                plugins: _,
                warnings,
            }) => {
                sources.write(error_w, warnings).map_err(Error::Io)?;
                let mut builder = LedgerBuilder::default();

                for directive in directives {
                    builder.directive(&directive);
                }
                Ok(builder)
            }

            Err(ParseError { errors, warnings }) => {
                sources.write(error_w, errors).map_err(Error::Io)?;
                sources.write(error_w, warnings).map_err(Error::Io)?;
                Err(Error::Parser)
            }
        }?;

        drop(parser);

        match builder.build(sources, error_w) {
            Err(errors) => Err(Error::Builder),

            Ok(ledger) => Ok(ledger),
        }
    }

    fn accounts(&self) -> std::collections::HashMap<String, Account> {
        // can't return a hashbrown::HashMap, but have to clone anyway, so this is not bad
        self.accounts
            .iter()
            .map(|(name, account)| (name.clone(), account.clone()))
            .collect()
    }
}

#[derive(Default, Debug)]
struct LedgerBuilder {
    open_accounts: HashMap<String, Span>,
    closed_accounts: HashMap<String, Span>,
    accounts: HashMap<String, AccountBuilder>,
    errors: Vec<parser::Error>,
}

impl LedgerBuilder {
    fn build<W>(self, sources: BeancountSources, error_w: W) -> Result<Ledger, Error>
    where
        W: Write + Copy,
    {
        if self.errors.is_empty() {
            Ok(Ledger {
                sources,
                accounts: self
                    .accounts
                    .into_iter()
                    .map(|(name, account)| (name, account.build()))
                    .collect(),
            })
        } else {
            sources.write(error_w, self.errors).map_err(Error::Io)?;
            Err(Error::Builder)
        }
    }

    fn directive(&mut self, directive: &Spanned<parser::Directive>) {
        use parser::DirectiveVariant::*;

        match directive.variant() {
            Transaction(transaction) => self.transaction(transaction, directive),
            Price(price) => self.price(price, directive),
            Balance(balance) => self.balance(balance, directive),
            Open(open) => self.open(open, directive),
            Close(close) => self.close(close, directive),
            Commodity(commodity) => self.commodity(commodity, directive),
            Pad(pad) => self.pad(pad, directive),
            Document(document) => self.document(document, directive),
            Note(note) => self.note(note, directive),
            Event(event) => self.event(event, directive),
            Query(query) => self.query(query, directive),
        }
    }

    fn transaction(
        &mut self,
        transaction: &parser::Transaction,
        directive: &Spanned<parser::Directive>,
    ) {
        for posting in transaction.postings() {
            self.post(posting);
        }
    }

    fn post(&mut self, posting: &Spanned<parser::Posting>) {
        if self
            .open_accounts
            .contains_key(&posting.account().item().to_string())
        {
            let account = self
                .accounts
                .get_mut(&posting.account().item().to_string())
                .unwrap();

            match (posting.amount(), posting.currency()) {
                (Some(amount), Some(currency)) => {
                    let currency = currency.to_string();
                    if account.currencies.contains(&currency) {
                        account
                            .postings
                            .push((Posting::new(amount.value(), currency), *posting.span()));
                    } else {
                        self.errors.push(posting.error_with_contexts(
                            "invalid currency for account",
                            vec![("open".to_string(), account.opened)],
                        ));
                    }
                }
                (None, Some(_)) => {
                    self.errors.push(posting.error("missing amount"));
                }
                (Some(_), None) => {
                    self.errors.push(posting.error("missing currency"));
                }
                (None, None) => {
                    self.errors
                        .push(posting.error("missing amount and currency"));
                }
            }
        } else if let Some(closed) = self
            .closed_accounts
            .get(&posting.account().item().to_string())
        {
            self.errors.push(
                posting.error_with_contexts(
                    "account was closed",
                    vec![("close".to_string(), *closed)],
                ),
            );
        } else {
            self.errors.push(posting.error("account not open"));
        }
    }

    fn price(&mut self, price: &parser::Price, directive: &Spanned<parser::Directive>) {}

    fn balance(&mut self, balance: &parser::Balance, directive: &Spanned<parser::Directive>) {}

    fn open(&mut self, open: &parser::Open, directive: &Spanned<parser::Directive>) {
        match self.open_accounts.entry(open.account().item().to_string()) {
            hash_map::Entry::Occupied(open_entry) => {
                self.errors.push(directive.error_with_contexts(
                    "account already opened",
                    vec![("open".to_string(), *open_entry.get())],
                ));
            }
            hash_map::Entry::Vacant(open_entry) => {
                let span = *directive.span();
                open_entry.insert(span);

                // cannot reopen a closed account
                if let Some(closed) = self.closed_accounts.get(&open.account().item().to_string()) {
                    self.errors.push(directive.error_with_contexts(
                        "account was closed",
                        vec![("close".to_string(), *closed)],
                    ));
                } else {
                    self.accounts.insert(
                        open.account().item().to_string(),
                        AccountBuilder::with_currencies(
                            open.currencies().map(|c| c.item().to_string()),
                            span,
                        ),
                    );
                }
            }
        }
    }

    fn close(&mut self, close: &parser::Close, directive: &Spanned<parser::Directive>) {
        match self.open_accounts.entry(close.account().item().to_string()) {
            hash_map::Entry::Occupied(open_entry) => {
                match self
                    .closed_accounts
                    .entry(close.account().item().to_string())
                {
                    hash_map::Entry::Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        self.errors.push(directive.error_with_contexts(
                            "account was already closed",
                            vec![("close".to_string(), *closed_entry.get())],
                        ));
                    }
                    hash_map::Entry::Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(*directive.span());
                    }
                }
            }
            hash_map::Entry::Vacant(_) => {
                self.errors.push(directive.error("account not open"));
            }
        }
    }

    fn commodity(&mut self, commodity: &parser::Commodity, directive: &Spanned<parser::Directive>) {
    }

    fn pad(&mut self, pad: &parser::Pad, directive: &Spanned<parser::Directive>) {}

    fn document(&mut self, document: &parser::Document, directive: &Spanned<parser::Directive>) {}

    fn note(&mut self, note: &parser::Note, directive: &Spanned<parser::Directive>) {}

    fn event(&mut self, event: &parser::Event, directive: &Spanned<parser::Directive>) {}

    fn query(&mut self, query: &parser::Query, directive: &Spanned<parser::Directive>) {}
}

#[derive(Clone, Debug, Steel)]
pub struct Account {
    pub(crate) currencies: HashSet<String>,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Posting>,
}

#[derive(Debug)]
pub struct AccountBuilder {
    pub(crate) currencies: HashSet<String>,
    pub(crate) opened: Span,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<(Posting, Span)>,
}

impl AccountBuilder {
    fn build(self) -> Account {
        Account {
            currencies: self.currencies,
            postings: self
                .postings
                .into_iter()
                .map(|(posting, span)| posting)
                .collect(),
        }
    }

    fn with_currencies<I>(currencies: I, opened: Span) -> Self
    where
        I: Iterator<Item = String>,
    {
        AccountBuilder {
            currencies: HashSet::from_iter(currencies),
            opened,
            postings: Vec::default(),
        }
    }
}

#[derive(Clone, Debug, Steel)]
pub struct Posting {
    pub(crate) amount: Amount,
    // TODO:
    // pub(crate) flag: Option<String>,
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl Posting {
    fn new(amount: Decimal, currency: String) -> Self {
        Posting {
            amount: Amount {
                number: Rational(amount),
                currency,
            },
        }
    }
}

#[derive(Clone, Debug, Steel)]
pub struct Amount {
    pub(crate) number: Rational,
    pub(crate) currency: String,
}

#[derive(Clone, Debug)]
pub struct Rational(Decimal);

impl Custom for Rational {}

impl Rational {
    fn numerator(&self) -> isize {
        self.0.mantissa() as isize
    }

    fn denominator(&self) -> isize {
        10isize.pow(self.0.scale())
    }
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parser,
    Builder,
    LedgerCog,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
            Builder => f.write_str("builder errors"),
            LedgerCog => f.write_str("error in Ledger cog"),
        }
    }
}

#[derive(Debug)]
pub enum BuilderError {
    AccountAlreadyOpen(Span),
    AccountNotOpen,
    AccountAlreadyClosed(Span),
    InvalidCurrencyForAccount(Span),
    MissingAmount,
    MissingCurrency,
}

impl Display for BuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BuilderError::*;

        match self {
            AccountAlreadyOpen(_) => f.write_str("account already open"),
            AccountNotOpen => f.write_str("account not open"),
            AccountAlreadyClosed(_) => f.write_str("account already closed"),
            InvalidCurrencyForAccount(_) => f.write_str("invalid currency for account"),
            MissingAmount => f.write_str("missing amount"),
            MissingCurrency => f.write_str("missing currency"),
        }
    }
}

pub fn register_types_and_functions(steel_engine: &mut Engine) {
    steel_engine.register_type::<Ledger>("Ledger?");
    steel_engine.register_fn("Ledger-accounts", Ledger::accounts);

    steel_engine.register_type::<Rational>("FFIRational?");
    steel_engine.register_fn("FFIRational-numerator", Rational::numerator);
    steel_engine.register_fn("FFIRational-denominator", Rational::denominator);
}
