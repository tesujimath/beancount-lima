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

use super::{types::*, Error};

impl Ledger {
    /// Empty ledger, for running cog tests only
    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Ledger {
            sources: BeancountSources::from(""),
            accounts: HashMap::default(),
        }
    }

    pub(crate) fn parse_from<W>(path: &Path, error_w: W) -> Result<Ledger, Error>
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
            self.post(*directive.date().item(), posting, None);
        }

        // auto-post if required
        if let Some(unspecified) = unspecified.pop() {
            for (currency, number) in residual {
                self.post(
                    *directive.date().item(),
                    unspecified,
                    Some((-number, currency.to_string()).into()),
                );
            }
        }
        // any other unspecified postings are errors
        for unspecified in unspecified.iter() {
            self.errors
                .push(unspecified.error("more than one posting without amount/currency"));
        }
    }

    fn post(
        &mut self,
        date: time::Date,
        posting: &Spanned<parser::Posting>,
        amount: Option<Amount>,
    ) {
        if self
            .open_accounts
            .contains_key(&posting.account().item().to_string())
        {
            let account = self
                .accounts
                .get_mut(&posting.account().item().to_string())
                .unwrap();

            // if amount is specified explicitly we use it, otherwise we extract it from the post
            let amount = amount.or_else(|| {
                let amount = posting.amount().map(|amount| amount.item().value());

                let currency = posting.currency().map(|currency| currency.item());

                match (amount, currency) {
                    (Some(amount), Some(currency)) => Some((amount, currency.to_string()).into()),
                    (None, Some(_)) => {
                        self.errors.push(posting.error("missing amount"));
                        None
                    }
                    (Some(_), None) => {
                        self.errors.push(posting.error("missing currency"));
                        None
                    }
                    (None, None) => {
                        self.errors
                            .push(posting.error("missing amount and currency"));
                        None
                    }
                }
            });

            if let Some(amount) = amount {
                use hashbrown::hash_map::Entry::*;

                if account.is_currency_valid(&amount.currency) {
                    match account.inventory.entry(amount.currency.clone()) {
                        Occupied(mut position) => {
                            let position = position.get_mut();
                            position.add(amount.number);
                        }
                        Vacant(position) => {
                            position.insert(amount.number);
                        }
                    }
                    account.postings.push(Posting::new(date.into(), amount));
                } else {
                    self.errors.push(posting.error_with_contexts(
                        "invalid currency for account",
                        vec![("open".to_string(), account.opened)],
                    ));
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

    fn pad(&mut self, pad: &parser::Pad, directive: &Spanned<parser::Directive>) {}

    fn document(&mut self, document: &parser::Document, directive: &Spanned<parser::Directive>) {}

    fn note(&mut self, note: &parser::Note, directive: &Spanned<parser::Directive>) {}

    fn event(&mut self, event: &parser::Event, directive: &Spanned<parser::Directive>) {}

    fn query(&mut self, query: &parser::Query, directive: &Spanned<parser::Directive>) {}
}

#[derive(Debug)]
struct AccountBuilder {
    // TODO support cost in inventory
    currencies: HashSet<String>,
    inventory: hashbrown::HashMap<String, Decimal>,
    opened: Span,
    // TODO
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    postings: Vec<Posting>,
}

impl AccountBuilder {
    fn build(self) -> Account {
        Account {
            postings: self.postings.into_iter().collect(),
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
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: &String) -> bool {
        self.currencies.is_empty() || self.currencies.contains(currency)
    }
}
