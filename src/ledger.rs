use beancount_parser_lima::{
    self as parser, spanned, BeancountParser, BeancountSources, ParseError, ParseSuccess, Span,
    Spanned,
};
use hashbrown::{hash_map, HashMap, HashSet};
use rust_decimal::Decimal;
use std::{
    fmt::Display,
    io::{self, Write},
    path::Path,
};
use steel::steel_vm::engine::Engine;
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
        };

        drop(parser);

        builder.map(|builder| builder.build(sources))
    }
}

#[derive(Default, Debug)]
struct LedgerBuilder {
    open_accounts: HashMap<String, Span>,
    closed_accounts: HashMap<String, Span>,
    accounts: HashMap<String, Account>,
    errors: Vec<Spanned<BuilderError>>,
}

impl LedgerBuilder {
    fn build(self, sources: BeancountSources) -> Ledger {
        Ledger {
            sources,
            accounts: self.accounts,
        }
    }

    fn directive(&mut self, directive: &Spanned<parser::Directive>) {
        use parser::DirectiveVariant::*;

        match directive.variant() {
            Transaction(transaction) => self.transaction(transaction),
            Price(price) => self.price(price),
            Balance(balance) => self.balance(balance),
            Open(open) => self.open(open, *directive.span()),
            Close(close) => self.close(close, *directive.span()),
            Commodity(commodity) => self.commodity(commodity),
            Pad(pad) => self.pad(pad),
            Document(document) => self.document(document),
            Note(note) => self.note(note),
            Event(event) => self.event(event),
            Query(query) => self.query(query),
        }
    }

    fn transaction(&mut self, transaction: &parser::Transaction) {}

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
                        account.postings.push(spanned(
                            Posting::new(amount.value(), currency),
                            *posting.span(),
                        ));
                    } else {
                        self.errors.push(spanned(
                            BuilderError::InvalidCurrencyForAccount(account.opened),
                            *posting.span(),
                        ));
                    }
                }
                (None, Some(_)) => {
                    self.errors
                        .push(spanned(BuilderError::MissingAmount, *posting.span()));
                }
                (Some(_), None) => {
                    self.errors
                        .push(spanned(BuilderError::MissingCurrency, *posting.span()));
                }
                (None, None) => {
                    self.errors
                        .push(spanned(BuilderError::MissingAmount, *posting.span()));
                    // and currency, but hey
                }
            }
        } else if let Some(closed) = self
            .closed_accounts
            .get(&posting.account().item().to_string())
        {
            self.errors.push(spanned(
                BuilderError::AccountAlreadyClosed(*closed),
                *posting.span(),
            ));
        } else {
            self.errors
                .push(spanned(BuilderError::AccountNotOpen, *posting.span()));
        }
    }

    fn price(&mut self, price: &parser::Price) {}

    fn balance(&mut self, balance: &parser::Balance) {}

    fn open(&mut self, open: &parser::Open, span: Span) {
        match self.open_accounts.entry(open.account().item().to_string()) {
            hash_map::Entry::Occupied(open_entry) => {
                self.errors.push(spanned(
                    BuilderError::AccountAlreadyOpen(*open_entry.get()),
                    span,
                ));
            }
            hash_map::Entry::Vacant(open_entry) => {
                open_entry.insert(span);

                // cannot reopen a closed account
                if let Some(closed) = self.closed_accounts.get(&open.account().item().to_string()) {
                    self.errors
                        .push(spanned(BuilderError::AccountAlreadyClosed(*closed), span));
                } else {
                    self.accounts.insert(
                        open.account().item().to_string(),
                        Account::with_currencies(
                            open.currencies().map(|c| c.item().to_string()),
                            span,
                        ),
                    );
                }
            }
        }
    }

    fn close(&mut self, close: &parser::Close, span: Span) {
        match self.open_accounts.entry(close.account().item().to_string()) {
            hash_map::Entry::Occupied(open_entry) => {
                match self
                    .closed_accounts
                    .entry(close.account().item().to_string())
                {
                    hash_map::Entry::Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        self.errors.push(spanned(
                            BuilderError::AccountAlreadyClosed(*closed_entry.get()),
                            span,
                        ));
                    }
                    hash_map::Entry::Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(span);
                    }
                }
            }
            hash_map::Entry::Vacant(_) => {
                self.errors
                    .push(spanned(BuilderError::AccountNotOpen, span));
            }
        }
    }

    fn commodity(&mut self, commodity: &parser::Commodity) {}

    fn pad(&mut self, pad: &parser::Pad) {}

    fn document(&mut self, document: &parser::Document) {}

    fn note(&mut self, note: &parser::Note) {}

    fn event(&mut self, event: &parser::Event) {}

    fn query(&mut self, query: &parser::Query) {}
}

#[derive(Clone, Debug, Steel)]
pub struct Account {
    pub(crate) currencies: HashSet<String>,
    pub(crate) opened: Span,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Spanned<Posting>>,
}

impl Account {
    fn with_currencies<I>(currencies: I, opened: Span) -> Self
    where
        I: Iterator<Item = String>,
    {
        Account {
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
                number: amount,
                currency,
            },
        }
    }
}

#[derive(Clone, Debug, Steel)]
pub struct Amount {
    pub(crate) number: Decimal,
    pub(crate) currency: String,
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parser,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
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

// TODO write BuilderError using BeancountSources and all references

pub fn register_types_and_functions(steel_engine: &mut Engine) {
    steel_engine.register_type::<Ledger>("Ledger");
}
