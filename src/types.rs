// TODO remove:
#![allow(dead_code, unused_variables)]
use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};
use steel::{
    rvals::{as_underlying_type, Custom, CustomType},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr, SteelVal,
};
use steel_derive::Steel;

#[derive(Clone, Steel, Debug)]
pub(crate) struct Account {
    // TODO
    // pub(crate) booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pub(crate) postings: Vec<Posting>,
}

impl Account {
    fn postings(&self) -> Vec<Posting> {
        self.postings.clone()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Account>("ffi-account?");
        steel_engine.register_fn("ffi-account-postings", Account::postings);
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

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
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

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
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

pub(crate) type Date = Wrapped<time::Date>;

impl Date {
    fn new(y: i32, m: i32, d: i32) -> steel::rvals::Result<Self> {
        u8::try_from(m)
            .ok()
            .and_then(|m| time::Month::try_from(m).ok())
            .and_then(|m| u8::try_from(d).ok().map(|d| (m, d)))
            .and_then(|(m, d)| time::Date::from_calendar_date(y, m, d).ok())
            .map(|date| date.into())
            .ok_or(SteelErr::new(
                steel::rerrs::ErrorKind::ConversionError,
                "bad date".to_string(),
            ))
    }

    fn parse(raw: String, strftime_format: String) -> steel::rvals::Result<Self> {
        let format_descr = time::format_description::parse_strftime_borrowed(&strftime_format)
            .map_err(|e| {
                SteelErr::new(
                    steel::rerrs::ErrorKind::ConversionError,
                    format!("bad date format: {}", e),
                )
            })?;
        time::Date::parse(&raw, &format_descr)
            .map_err(|e| {
                SteelErr::new(
                    steel::rerrs::ErrorKind::ConversionError,
                    format!("bad date: {}", e),
                )
            })
            .map(Self)
    }

    // beginning of time
    fn bot() -> Date {
        time::Date::MIN.into()
    }

    // end of time
    fn eot() -> Date {
        time::Date::MAX.into()
    }

    // positive or negative offset in days
    fn after(&self, d: isize) -> steel::rvals::Result<Self> {
        if d >= 0 {
            self.0.checked_add(time::Duration::days(d as i64))
        } else {
            self.0.checked_sub(time::Duration::days(-d as i64))
        }
        .map_or(
            Err(SteelErr::new(
                steel::rerrs::ErrorKind::ConversionError,
                "date overflow".to_string(),
            )),
            |date| Ok(date.into()),
        )
    }

    // positive or negative offset in days
    fn before(&self, d: isize) -> steel::rvals::Result<Self> {
        self.after(-d)
    }

    // Julian day
    fn julian(&self) -> i32 {
        self.0.to_julian_day()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Date>("date?");
        steel_engine.register_fn("date", Date::new);
        steel_engine.register_fn("date-bot", Date::bot);
        steel_engine.register_fn("date-eot", Date::eot);
        steel_engine.register_fn("date=?", Date::eq);
        steel_engine.register_fn("date>?", Date::gt);
        steel_engine.register_fn("date<?", Date::lt);
        steel_engine.register_fn("date>=?", Date::ge);
        steel_engine.register_fn("date<=?", Date::le);
        steel_engine.register_fn("parse-date", Date::parse);
        steel_engine.register_fn("date-after", Date::after);
        steel_engine.register_fn("date-before", Date::before);
        steel_engine.register_fn("date-julian", Date::julian);
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Decimal(rust_decimal::Decimal);

impl Decimal {
    pub(crate) fn add(&mut self, other: Decimal) -> Decimal {
        (self.0 + other.0).into()
    }

    fn numerator(&self) -> isize {
        self.0.mantissa() as isize
    }

    fn denominator(&self) -> isize {
        10isize.pow(self.0.scale())
    }

    fn new(m: i64, e: u32) -> Self {
        rust_decimal::Decimal::new(m, e).into()
    }

    fn zero() -> Self {
        rust_decimal::Decimal::ZERO.into()
    }

    fn is_zero(&self) -> bool {
        self.0 == rust_decimal::Decimal::ZERO
    }

    // width of digits and/or sign to left of decimal point
    fn width_left(&self) -> u32 {
        let sign_width = if self.0.is_sign_negative() { 1u32 } else { 0 };
        let mut mantissa_width = 0u32;
        let mut abs_mantissa = self.0.mantissa().abs();
        while abs_mantissa > 0 {
            abs_mantissa /= 10;
            mantissa_width += 1;
        }

        if sign_width + mantissa_width > self.0.scale() {
            sign_width + mantissa_width - self.0.scale()
        } else {
            1
        }
    }

    // width of digits  to right of decimal point
    fn width_right(&self) -> u32 {
        self.0.scale()
    }

    fn parse(raw: String) -> steel::rvals::Result<Self> {
        rust_decimal::Decimal::from_str_exact(&raw)
            .map_err(|e| SteelErr::new(steel::rerrs::ErrorKind::ConversionError, e.to_string()))
            .map(Self)
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Decimal>("decimal?");
        steel_engine.register_fn("decimal", Decimal::new);
        steel_engine.register_fn("decimal=?", Decimal::eq);
        steel_engine.register_fn("decimal>?", Decimal::gt);
        steel_engine.register_fn("decimal<?", Decimal::lt);
        steel_engine.register_fn("decimal>=?", Decimal::ge);
        steel_engine.register_fn("decimal<=?", Decimal::le);
        steel_engine.register_fn("decimal-zero", Decimal::zero);
        steel_engine.register_fn("decimal-zero?", Decimal::is_zero);
        steel_engine.register_fn("decimal-add", Decimal::add);
        steel_engine.register_fn("decimal->string", Decimal::to_string);
        steel_engine.register_fn("decimal-numerator", Decimal::numerator);
        steel_engine.register_fn("decimal-denominator", Decimal::denominator);
        steel_engine.register_fn("decimal-width-left", Decimal::width_left);
        steel_engine.register_fn("decimal-width-right", Decimal::width_right);
        steel_engine.register_fn("parse-decimal", Decimal::parse);
    }
}

impl From<rust_decimal::Decimal> for Decimal {
    fn from(value: rust_decimal::Decimal) -> Self {
        Self(value)
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Eq for Decimal {}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Ord for Decimal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
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

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Decimal>(other) {
            self == other
        } else {
            false
        }
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

impl<T> PartialEq for Wrapped<T>
where
    T: Clone + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> PartialOrd for Wrapped<T>
where
    T: Clone + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Eq for Wrapped<T> where T: Clone + Eq {}

impl<T> Ord for Wrapped<T>
where
    T: Clone + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
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

#[derive(Clone, Steel, Debug)]
pub(crate) struct AlistItem {
    key: String,
    value: SteelVal,
}

impl Display for AlistItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} . {})", &self.key, &self.value)
    }
}

impl AlistItem {
    fn key(&self) -> String {
        self.key.clone()
    }

    fn value(&self) -> SteelVal {
        self.value.clone()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<AlistItem>("ffi-alistitem?");
        steel_engine.register_fn("ffi-alistitem-key", AlistItem::key);
        steel_engine.register_fn("ffi-alistitem-value", AlistItem::value);
    }
}

impl<K, V> From<(K, V)> for AlistItem
where
    K: AsRef<str>,
    V: AsRef<str>,
{
    fn from(value: (K, V)) -> Self {
        AlistItem {
            key: value.0.as_ref().to_string(),
            value: SteelVal::StringV(value.1.as_ref().to_string().into()),
        }
    }
}
