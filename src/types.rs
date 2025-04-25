// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::BeancountSources;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::{Deref, DerefMut},
};
use steel::{
    rvals::Custom,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use steel_derive::Steel;

#[derive(Clone, Debug, Steel)]
pub(crate) struct Ledger {
    pub(crate) sources: BeancountSources,
    pub(crate) currencies: HashSet<String>,
    pub(crate) accounts: HashMap<String, Account>,
}

impl Ledger {
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

    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Ledger>("ffi-ledger?");
        steel_engine.register_fn("ffi-ledger-currencies", Ledger::currencies);
        steel_engine.register_fn("ffi-ledger-account-names", Ledger::account_names);
        steel_engine.register_fn("ffi-ledger-accounts", Ledger::accounts);
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Account {
    // TODO support cost in the inventory
    pub(crate) inventory: HashMap<String, Decimal>,
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Posting>,
}

impl Account {
    fn inventory(&self) -> HashMap<String, Decimal> {
        self.inventory.clone()
    }

    fn postings(&self) -> Vec<Posting> {
        self.postings.clone()
    }

    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Account>("ffi-account?");
        steel_engine.register_fn("ffi-account->string", Account::to_string);
        steel_engine.register_fn("ffi-account-inventory", Account::inventory);
        steel_engine.register_fn("ffi-account-postings", Account::postings);
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

#[derive(Clone, Debug)]
pub(crate) struct Posting {
    pub(crate) date: Date,
    pub(crate) amount: Amount,
    // TODO:
    // pub(crate) flag: Option<String>,
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl Posting {
    pub(crate) fn new(date: Date, amount: Amount) -> Self {
        Posting { date, amount }
    }

    fn date(&self) -> Date {
        self.date.clone()
    }

    fn amount(&self) -> Amount {
        self.amount.clone()
    }

    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Posting>("ffi-posting?");
        steel_engine.register_fn("ffi-posting->string", Posting::to_string);
        steel_engine.register_fn("ffi-posting-date", Posting::date);
        steel_engine.register_fn("ffi-posting-amount", Posting::amount);
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
pub(crate) struct Amount {
    pub(crate) number: Decimal,
    pub(crate) currency: String,
}

// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn number(&self) -> Decimal {
        self.number
    }

    fn currency(&self) -> String {
        self.currency.clone()
    }

    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Amount>("ffi-amount?");
        steel_engine.register_fn("ffi-amount->string", Amount::to_string);
        steel_engine.register_fn("ffi-amount-number", Amount::number);
        steel_engine.register_fn("ffi-amount-currency", Amount::currency);
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

impl From<(rust_decimal::Decimal, String)> for Amount {
    fn from(value: (rust_decimal::Decimal, String)) -> Self {
        Amount {
            number: value.0.into(),
            currency: value.1,
        }
    }
}

type Date = Wrapped<time::Date>;

impl Date {
    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Date>("date?");
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Decimal(rust_decimal::Decimal);

impl Decimal {
    pub(crate) fn add(&mut self, other: Decimal) {
        self.0 += other.0;
    }

    fn numerator(&self) -> isize {
        self.0.mantissa() as isize
    }

    fn denominator(&self) -> isize {
        10isize.pow(self.0.scale())
    }

    fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Decimal>("decimal?");
        steel_engine.register_fn("decimal->string", Decimal::to_string);
        steel_engine.register_fn("decimal-numerator", Decimal::numerator);
        steel_engine.register_fn("decimal-denominator", Decimal::denominator);
    }
}

impl From<rust_decimal::Decimal> for Decimal {
    fn from(value: rust_decimal::Decimal) -> Self {
        Self(value)
    }
}

impl Display for Decimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Custom for Decimal {
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

pub(crate) fn register_types_with_engine(steel_engine: &mut Engine) {
    Account::register_with_engine(steel_engine);
    Amount::register_with_engine(steel_engine);
    Date::register_with_engine(steel_engine);
    Decimal::register_with_engine(steel_engine);
    Ledger::register_with_engine(steel_engine);
    Posting::register_with_engine(steel_engine);
}
