// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Span, Spanned,
};
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::Path,
};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_derive::Steel;

use super::{types::*, Error};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Ledger {
    pub(crate) sources: BeancountSources,
    pub(crate) accounts: HashMap<String, Account>,
}

impl Ledger {
    /// Empty ledger, for running cog tests only
    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Ledger {
            sources: BeancountSources::from(""),
            accounts: HashMap::default(),
        }
    }

    pub(crate) fn parse_from<W>(path: &Path, error_w: W) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        let sources = BeancountSources::try_from(path).map_err(Error::Io)?;
        let parser = BeancountParser::new(&sources);

        let mut builder = match parser.parse() {
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

        builder.validate();

        match builder.build(sources, error_w) {
            Err(errors) => Err(Error::Builder),

            Ok(ledger) => Ok(ledger),
        }
    }

    fn accounts(&self) -> HashMap<String, Account> {
        self.accounts.clone()
    }

    pub(crate) fn register(self, steel_engine: &mut Engine) {
        steel_engine
            .register_external_value("*ffi-ledger*", self)
            .unwrap(); // can't fail
    }
}

#[derive(Default, Debug)]
struct LedgerBuilder {
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<String, Span>,
    closed_accounts: hashbrown::HashMap<String, Span>,
    accounts: HashMap<String, AccountBuilder>,
    errors: Vec<parser::Error>,
}

impl LedgerBuilder {
    // generate any errors before building
    fn validate(&mut self) {
        // check for unused pad directives
        for account in self.accounts.values() {
            if let Some(pad) = &account.pad {
                self.errors.push(pad.error("unused"))
            }
        }
    }

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
        // determine auto-posting for at most one unspecified account
        let mut residual =
            hashbrown::HashMap::<&parser::Currency<'_>, rust_decimal::Decimal>::default();
        let mut unspecified: Vec<&Spanned<parser::Posting<'_>>> = Vec::default();

        for posting in transaction.postings() {
            use hashbrown::hash_map::Entry::*;

            if let (Some(amount), Some(currency)) = (posting.amount(), posting.currency()) {
                match residual.entry(currency.item()) {
                    Occupied(mut residual_entry) => {
                        let accumulated = residual_entry.get() + amount.value();
                        residual_entry.insert(accumulated);
                    }
                    Vacant(residual_entry) => {
                        residual_entry.insert(amount.value());
                    }
                }
            } else {
                unspecified.push(posting);
            }
        }

        // record each posting for which we have both amount and currency
        for posting in transaction
            .postings()
            .filter(|posting| posting.amount().is_some() && posting.currency().is_some())
        {
            let amount = posting.amount().unwrap().item().value();
            let currency = posting.currency().unwrap().item();
            self.post(
                (*directive.date().item()).into(),
                &posting.account().to_string(),
                (amount, currency.to_string()).into(),
                posting.flag().map(|flag| flag.item().to_string()),
                posting,
            );
        }

        // auto-post if required
        if let Some(unspecified) = unspecified.pop() {
            for (currency, number) in residual {
                self.post(
                    (*directive.date().item()).into(),
                    &unspecified.account().to_string(),
                    (-number, currency.to_string()).into(),
                    unspecified.flag().map(|flag| flag.item().to_string()),
                    unspecified,
                );
            }
        }
        // any other unspecified postings are errors
        for unspecified in unspecified.iter() {
            self.errors
                .push(unspecified.error("more than one posting without amount/currency"));
        }
    }

    fn post<E>(
        &mut self,
        date: Date,
        account_name: &str,
        amount: Amount,
        flag: Option<String>,
        source: &Spanned<E>,
    ) where
        E: parser::ElementType,
    {
        if self.open_accounts.contains_key(account_name) {
            let account = self.accounts.get_mut(account_name).unwrap();

            use hashbrown::hash_map::Entry::*;

            if account.is_currency_valid(&amount.currency) {
                match account.inventory.entry(amount.currency.clone()) {
                    Occupied(mut position) => {
                        let value = position.get_mut();
                        *value = value.add(amount.number);

                        tracing::debug!(
                            "post {} {} value for {} is now {}",
                            &date,
                            &amount,
                            &account_name,
                            value,
                        );

                        if value.is_zero() {
                            position.remove_entry();
                        }
                    }
                    Vacant(position) => {
                        position.insert(amount.number);
                    }
                }
                account.postings.push(Posting::new(date, amount, flag));
            } else {
                self.errors.push(source.error_with_contexts(
                    "invalid currency for account",
                    vec![("open".to_string(), account.opened)],
                ));
            }
        } else if let Some(closed) = self.closed_accounts.get(account_name) {
            self.errors.push(
                source.error_with_contexts(
                    "account was closed",
                    vec![("close".to_string(), *closed)],
                ),
            );
        } else {
            self.errors.push(source.error("account not open"));
        }
    }

    fn price(&mut self, price: &parser::Price, directive: &Spanned<parser::Directive>) {}

    fn balance(&mut self, balance: &parser::Balance, directive: &Spanned<parser::Directive>) {
        let account_name = balance.account().item().to_string();
        let (margin, pad) = if self.open_accounts.contains_key(&account_name) {
            let account = self.accounts.get_mut(&account_name).unwrap();

            // what's the gap between what we have and what the balance says we should have?
            let margin = account
                .inventory
                .iter()
                .map(|(cur, number)| {
                    if balance.atol().amount().currency().item().as_ref() == cur.as_str() {
                        (
                            cur,
                            balance.atol().amount().number().item().value()
                                - Into::<rust_decimal::Decimal>::into(*number),
                        )
                    } else {
                        (cur, -(Into::<rust_decimal::Decimal>::into(*number)))
                    }
                })
                .filter_map(|(cur, number)| {
                    // discard anything below the tolerance
                    (number.abs()
                        > balance
                            .atol()
                            .tolerance()
                            .map(|x| *x.item())
                            .unwrap_or(rust_decimal::Decimal::ZERO))
                    .then_some((cur.to_string(), number))
                })
                .collect::<HashMap<_, _>>();

            // pad can't last beyond balance
            ((!margin.is_empty()).then_some(margin), account.pad.take())
        } else {
            self.errors.push(directive.error("account not open"));
            (None, None)
        };

        match (margin, pad) {
            (Some(margin), Some(pad)) => {
                tracing::debug!(
                    "margin {}",
                    margin
                        .iter()
                        .map(|(cur, number)| format!("{} {}", -number, cur))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                for (cur, number) in &margin {
                    let amount = Into::<Amount>::into((*number, cur.to_string()));
                    let pad_flag = Some("P".to_string());
                    tracing::debug!("pad {} {} {}", pad.date.clone(), &account_name, &amount,);
                    self.post(pad.date.clone(), &account_name, amount, pad_flag, &pad);
                }
            }
            (Some(margin), None) => {
                self.errors.push(directive.error(format!(
                            "margin is {}",
                            &margin
                                .into_iter()
                                .map(|(cur, number)| format!("{} {}", -number, cur))
                                .collect::<Vec<String>>()
                                .join(", ")
                        )));
            }
            (None, Some(pad)) => {}
            (None, None) => {}
        }
    }

    fn open(&mut self, open: &parser::Open, directive: &Spanned<parser::Directive>) {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(open.account().item().to_string()) {
            Occupied(open_entry) => {
                self.errors.push(directive.error_with_contexts(
                    "account already opened",
                    vec![("open".to_string(), *open_entry.get())],
                ));
            }
            Vacant(open_entry) => {
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
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(close.account().item().to_string()) {
            Occupied(open_entry) => {
                match self
                    .closed_accounts
                    .entry(close.account().item().to_string())
                {
                    Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        self.errors.push(directive.error_with_contexts(
                            "account was already closed",
                            vec![("close".to_string(), *closed_entry.get())],
                        ));
                    }
                    Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(*directive.span());
                    }
                }
            }
            Vacant(_) => {
                self.errors.push(directive.error("account not open"));
            }
        }
    }

    fn commodity(&mut self, commodity: &parser::Commodity, directive: &Spanned<parser::Directive>) {
    }

    fn pad(&mut self, pad: &parser::Pad, directive: &Spanned<parser::Directive>) {
        let account_name = pad.account().item().to_string();
        if self.open_accounts.contains_key(&account_name) {
            let account = self.accounts.get_mut(&account_name).unwrap();
            let unused_pad = account.pad.replace(parser::spanned(
                Pad::new(directive.date().item(), pad.source()),
                *directive.span(),
            ));

            // unused pad directives are errors
            // https://beancount.github.io/docs/beancount_language_syntax.html#unused-pad-directives
            if let Some(unused_pad) = unused_pad {
                self.errors.push(unused_pad.error("unused"));
            }
        } else {
            self.errors.push(directive.error("account not open"));
        }
    }

    fn document(&mut self, document: &parser::Document, directive: &Spanned<parser::Directive>) {}

    fn note(&mut self, note: &parser::Note, directive: &Spanned<parser::Directive>) {}

    fn event(&mut self, event: &parser::Event, directive: &Spanned<parser::Directive>) {}

    fn query(&mut self, query: &parser::Query, directive: &Spanned<parser::Directive>) {}
}

#[derive(Debug)]
struct AccountBuilder {
    // TODO support cost in inventory
    currencies: HashSet<String>,
    inventory: hashbrown::HashMap<String, Decimal>, // only non-zero positions are maintained
    opened: Span,
    // TODO
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    postings: Vec<Posting>,
    pad: Option<parser::Spanned<Pad>>, // the string is the pad account
}

impl AccountBuilder {
    fn build(self) -> Account {
        // sort posting by date because pad posts were inserted at the balance directive not the pad
        let mut postings = self.postings;
        postings.sort_by_key(|post| post.date.julian());
        Account {
            postings: postings.into_iter().collect(),
        }
    }

    fn with_currencies<I>(currencies: I, opened: Span) -> Self
    where
        I: Iterator<Item = String>,
    {
        AccountBuilder {
            currencies: currencies.collect(),
            inventory: hashbrown::HashMap::default(),
            opened,
            postings: Vec::default(),
            pad: None,
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: &String) -> bool {
        self.currencies.is_empty() || self.currencies.contains(currency)
    }
}

#[derive(Debug)]
struct Pad {
    date: Date,
    source: String,
}

impl Pad {
    fn new(date: &time::Date, source: &parser::Account) -> Self {
        Pad {
            date: (*date).into(),
            source: source.to_string(),
        }
    }
}

impl parser::ElementType for Pad {
    fn element_type(&self) -> &'static str {
        "pad"
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<Ledger>("ffi-ledger?");
    steel_engine.register_fn("ffi-ledger-accounts", Ledger::accounts);
}
