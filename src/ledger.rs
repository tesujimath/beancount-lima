// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Span, Spanned,
};
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io::{self, Write},
    ops::{Deref, DerefMut},
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
    currencies: HashSet<String>,
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

    fn currencies(&self) -> Vec<String> {
        let mut currencies = self.currencies.iter().cloned().collect::<Vec<_>>();
        currencies.sort();
        currencies
    }

    fn account_names(&self) -> Vec<String> {
        let mut account_names = self.accounts.keys().cloned().collect::<Vec<_>>();
        account_names.sort();
        account_names
    }

    fn accounts(&self) -> HashMap<String, Account> {
        self.accounts.clone()
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
            // determine all currencies actually used
            let mut currencies = HashSet::<&str>::default();
            for account in self.accounts.values() {
                for currency in account.inventory.keys() {
                    currencies.insert(currency.as_str());
                }
            }
            let currencies = currencies.into_iter().map(|s| s.to_string()).collect();

            Ok(Ledger {
                sources,
                currencies,
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
        let mut residual = hashbrown::HashMap::<&parser::Currency<'_>, Decimal>::default();
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
            self.post(*directive.date().item(), posting, None, None);
        }

        // auto-post if required
        if let Some(unspecified) = unspecified.pop() {
            for (currency, amount) in residual {
                self.post(
                    *directive.date().item(),
                    unspecified,
                    Some(-amount),
                    Some(currency),
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
        default_amount: Option<Decimal>,
        default_currency: Option<&parser::Currency>,
    ) {
        if self
            .open_accounts
            .contains_key(&posting.account().item().to_string())
        {
            let account = self
                .accounts
                .get_mut(&posting.account().item().to_string())
                .unwrap();

            let amount = posting
                .amount()
                .map(|amount| amount.item().value())
                .or(default_amount);

            let currency = posting
                .currency()
                .map(|currency| currency.item())
                .or(default_currency);

            match (amount, currency) {
                (Some(amount), Some(currency)) => {
                    use hashbrown::hash_map::Entry::*;
                    let currency = currency.to_string();

                    if account.is_currency_valid(&currency) {
                        account
                            .postings
                            .push(Posting::new(date, amount, currency.clone()));
                        match account.inventory.entry(currency) {
                            Occupied(mut position) => {
                                let position = position.get_mut();
                                position.add_decimal(amount);
                            }
                            Vacant(position) => {
                                position.insert(amount.into());
                            }
                        }
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

#[derive(Clone, Debug)]
pub struct Account {
    // TODO support cost in the inventory
    pub(crate) inventory: HashMap<String, Rational>,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Posting>,
}

impl Account {
    fn inventory(&self) -> HashMap<String, Rational> {
        self.inventory.clone()
    }

    fn postings(&self) -> Vec<Posting> {
        self.postings.clone()
    }
}

impl Display for Account {
    // display the inventory like a Clojure literal hash
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pad = "";
        f.write_str("{")?;
        // sort so output is deterministic
        let mut currencies = self.inventory.keys().collect::<Vec<_>>();
        currencies.sort();
        for currency in currencies.into_iter() {
            write!(
                f,
                "{}\"{}\" {}",
                pad,
                currency,
                self.inventory.get(currency).unwrap()
            )?;
            pad = ", ";
        }
        f.write_str("}")?;

        Ok(())
    }
}

impl Custom for Account {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

#[derive(Debug)]
pub struct AccountBuilder {
    // TODO support cost in inventory
    pub(crate) currencies: HashSet<String>,
    pub(crate) inventory: hashbrown::HashMap<String, Rational>,
    pub(crate) opened: Span,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Posting>,
}

impl AccountBuilder {
    fn build(self) -> Account {
        Account {
            inventory: self.inventory.into_iter().collect(),
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

#[derive(Clone, Debug)]
pub struct Posting {
    pub(crate) date: Date,
    pub(crate) amount: Amount,
    // TODO:
    // pub(crate) flag: Option<String>,
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl Posting {
    fn new(date: time::Date, amount: Decimal, currency: String) -> Self {
        Posting {
            date: date.into(),
            amount: Amount {
                number: Rational(amount),
                currency,
            },
        }
    }

    fn date(&self) -> Date {
        self.date.clone()
    }

    fn amount(&self) -> Amount {
        self.amount.clone()
    }
}

impl Display for Posting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.date, &self.amount)
    }
}

impl Custom for Posting {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

#[derive(Clone, Debug)]
pub struct Amount {
    pub(crate) number: Rational,
    pub(crate) currency: String,
}

// TODO derive getters one this is resolved:
// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn number(&self) -> Rational {
        self.number.clone()
    }

    fn currency(&self) -> String {
        self.currency.clone()
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.number, &self.currency)
    }
}

impl Custom for Amount {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

type Date = Wrapped<time::Date>;

#[derive(Clone, Debug)]
pub struct Rational(Decimal);

impl Rational {
    fn add_decimal(&mut self, x: Decimal) {
        self.0 += x;
    }

    fn numerator(&self) -> isize {
        self.0.mantissa() as isize
    }

    fn denominator(&self) -> isize {
        10isize.pow(self.0.scale())
    }
}

impl From<Decimal> for Rational {
    fn from(value: Decimal) -> Self {
        Self(value)
    }
}

impl Display for Rational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Custom for Rational {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Wrapped<T>(T)
where
    T: Clone;

impl<T> Custom for Wrapped<T>
where
    T: Clone + Display + 'static,
{
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }
}

impl<T> From<T> for Wrapped<T>
where
    T: Clone,
{
    fn from(value: T) -> Self {
        Wrapped(value)
    }
}

impl<T> Display for Wrapped<T>
where
    T: Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Deref for Wrapped<T>
where
    T: Clone,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Wrapped<T>
where
    T: Clone,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parser,
    Builder,
    Scheme,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
            Builder => f.write_str("builder errors"),
            Scheme => f.write_str("error in Scheme"),
        }
    }
}

pub fn register_types_and_functions(steel_engine: &mut Engine) {
    steel_engine.register_type::<Ledger>("Ledger?");
    steel_engine.register_fn("Ledger-currencies", Ledger::currencies);
    steel_engine.register_fn("Ledger-account-names", Ledger::account_names);
    steel_engine.register_fn("Ledger-accounts", Ledger::accounts);

    steel_engine.register_type::<Account>("Account?");
    steel_engine.register_fn("Account->string", Account::to_string);
    steel_engine.register_fn("Account-inventory", Account::inventory);
    steel_engine.register_fn("Account-postings", Account::postings);

    steel_engine.register_type::<Posting>("Posting?");
    steel_engine.register_fn("Posting->string", Posting::to_string);
    steel_engine.register_fn("Posting-date", Posting::date);
    steel_engine.register_fn("Posting-amount", Posting::amount);

    steel_engine.register_type::<Amount>("Amount?");
    steel_engine.register_fn("Amount->string", Amount::to_string);
    steel_engine.register_fn("Amount-number", Amount::number);
    steel_engine.register_fn("Amount-currency", Amount::currency);

    steel_engine.register_type::<Rational>("FFIRational?");
    steel_engine.register_fn("FFIRational->string", Rational::to_string);
    steel_engine.register_fn("FFIRational-numerator", Rational::numerator);
    steel_engine.register_fn("FFIRational-denominator", Rational::denominator);

    steel_engine.register_fn("tabulate", crate::tabulate::tabulate);
}
