// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser, BeancountSources, ElementType, Span, Spanned};
use color_eyre::eyre::Result;
use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex, MutexGuard},
};
use steel::{
    rvals::{as_underlying_type, Custom, CustomType, SteelString, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr, SteelVal,
};
use steel_derive::Steel;

#[derive(Clone, Debug)]
pub(crate) struct Directive {
    pub(crate) date: Date,
    pub(crate) element: WrappedSpannedElement,
    pub(crate) variant: DirectiveVariant,
}

impl Custom for Directive {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

#[derive(Clone, Steel, Debug)]
pub(crate) enum DirectiveVariant {
    Transaction(Transaction),
    Price(Price),
    Balance(Balance),
    Open(Open),
    Close(Close),
    Commodity(Commodity),
    Pad(Pad),
    Document(Document),
    Note(Note),
    Event(Event),
    Query(Query),
}

// Scheme is not statically typed, so we don't force the user to unpack the directive variants, but rather support direct access
impl Directive {
    fn is_transaction(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Transaction(_))
    }
    fn transaction_postings(&self) -> steel::rvals::Result<SteelVal> {
        if let DirectiveVariant::Transaction(x) = &self.variant {
            Ok(SteelVal::VectorV(x.postings.clone()))
        } else {
            Err(SteelErr::new(
                steel::rerrs::ErrorKind::TypeMismatch,
                format!(
                    "can't call transaction_postings on {}",
                    self.element.element_type()
                ),
            ))
        }
    }
    // TODO other accessors

    fn is_price(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Price(_))
    }
    fn is_balance(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Balance(_))
    }
    fn is_open(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Open(_))
    }
    fn is_close(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Close(_))
    }
    fn is_commodity(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Commodity(_))
    }
    fn is_pad(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Pad(_))
    }
    fn is_document(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Document(_))
    }
    fn is_note(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Note(_))
    }
    fn is_event(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Event(_))
    }
    fn is_query(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Query(_))
    }
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Transaction {
    pub(crate) flag: SteelString,
    pub(crate) payee: Option<SteelString>,
    pub(crate) narration: Option<SteelString>,
    pub(crate) postings: SteelVector,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Price {}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Balance {
    pub(crate) account: SteelString,
    pub(crate) amount: Amount,
    pub(crate) tolerance: Option<Decimal>,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Open {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Close {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Commodity {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Pad {
    pub(crate) source: SteelString,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Document {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Note {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Event {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Query {
    // TODO
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Posting {
    pub(crate) flag: Option<SteelString>,
    pub(crate) account: SteelString,
    pub(crate) amount: Amount,
    // TODO:
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl Posting {
    pub(crate) fn new<S1, S2>(account: S1, amount: Amount, flag: Option<S2>) -> Self
    where
        S1: Display,
        S2: Display,
    {
        Posting {
            account: account.to_string().into(),
            amount,
            flag: flag.map(|flag| flag.to_string().into()),
        }
    }

    pub(crate) fn create(flag: Option<SteelString>, account: SteelString, amount: Amount) -> Self {
        Posting {
            account,
            amount,
            flag,
        }
    }

    fn account(&self) -> SteelString {
        self.account.clone()
    }

    fn amount(&self) -> Amount {
        self.amount.clone()
    }

    fn flag(&self) -> Option<SteelString> {
        self.flag.clone()
    }
}

impl Custom for Posting {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Posting>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Amount {
    pub(crate) number: Decimal,
    pub(crate) currency: SteelString,
}

// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn new(number: Decimal, currency: SteelString) -> Self {
        Self { number, currency }
    }

    fn number(&self) -> Decimal {
        self.number
    }

    fn currency(&self) -> SteelString {
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

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Amount>(other) {
            self == other
        } else {
            false
        }
    }
}

impl<S> From<(rust_decimal::Decimal, S)> for Amount
where
    S: Display,
{
    fn from(value: (rust_decimal::Decimal, S)) -> Self {
        Amount {
            number: value.0.into(),
            currency: value.1.to_string().into(),
        }
    }
}

impl From<&parser::Amount<'_>> for Amount {
    fn from(value: &parser::Amount) -> Self {
        (value.number().value(), value.currency()).into()
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
                    format!("bad date format: {e}"),
                )
            })?;
        time::Date::parse(&raw, &format_descr)
            .map_err(|e| {
                SteelErr::new(
                    steel::rerrs::ErrorKind::ConversionError,
                    format!("bad date: {e}"),
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
    pub(crate) fn julian(&self) -> i32 {
        self.0.to_julian_day()
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Decimal(rust_decimal::Decimal);

impl Decimal {
    pub(crate) fn add(&self, other: Decimal) -> Decimal {
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

    pub(crate) fn is_zero(&self) -> bool {
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

    /// parse a decimal forcing at least 2 decimal places
    fn parse_cents(raw: String) -> steel::rvals::Result<Self> {
        Self::parse(raw).map(|Self(mut d)| {
            if d.scale() < 2 {
                d.rescale(2);
            }
            Self(d)
        })
    }
}

impl From<rust_decimal::Decimal> for Decimal {
    fn from(value: rust_decimal::Decimal) -> Self {
        Self(value)
    }
}

impl From<Decimal> for rust_decimal::Decimal {
    fn from(value: Decimal) -> Self {
        value.0
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

impl<T> Copy for Wrapped<T> where T: Clone + Copy {}

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

#[derive(Clone, Debug)]
pub struct MutexWrapper<T>(Arc<Mutex<T>>);

impl<T> MutexWrapper<T> {
    pub fn new(item: T) -> Self {
        MutexWrapper(Arc::new(Mutex::new(item)))
    }

    pub fn lock(&self) -> MutexGuard<T> {
        self.0.lock().unwrap()
    }
}

impl<T> Custom for MutexWrapper<T> where T: Clone + Display + 'static {}

#[derive(Clone, Debug, Steel)]
pub struct WrappedBeancountSources(Arc<Mutex<BeancountSources>>);

impl From<BeancountSources> for WrappedBeancountSources {
    fn from(sources: BeancountSources) -> Self {
        WrappedBeancountSources(Arc::new(Mutex::new(sources)))
    }
}

impl WrappedBeancountSources {
    pub fn lock(&self) -> MutexGuard<BeancountSources> {
        self.0.lock().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    element_type: &'static str,
}

impl Element {
    pub(crate) fn new(element_type: &'static str, span: Span) -> Spanned<Self> {
        parser::spanned(Element { element_type }, span)
    }
}

impl parser::ElementType for Element {
    fn element_type(&self) -> &'static str {
        self.element_type
    }
}

#[derive(Clone, Debug)]
pub struct WrappedSpannedElement(Arc<Spanned<Element>>);

impl Custom for WrappedSpannedElement {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(format!("Element::{}", self.0.as_ref().element_type())))
    }
}

impl<T> From<&Spanned<T>> for WrappedSpannedElement
where
    T: parser::ElementType,
{
    fn from(spanned_element: &Spanned<T>) -> Self {
        WrappedSpannedElement(Arc::new(parser::spanned(
            Element {
                element_type: spanned_element.element_type(),
            },
            *spanned_element.span(),
        )))
    }
}

impl parser::ElementType for WrappedSpannedElement {
    fn element_type(&self) -> &'static str {
        self.0.element_type
    }
}

impl WrappedSpannedElement {
    pub(crate) fn error<S>(&self, message: S) -> parser::AnnotatedError
    where
        S: Into<String>,
    {
        self.0.as_ref().error(message).into()
    }

    pub(crate) fn annotated_error<S1, S2>(
        &self,
        message: S1,
        annotation: S2,
    ) -> parser::AnnotatedError
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        self.0.as_ref().error(message).with_annotation(annotation)
    }

    pub(crate) fn error_with_contexts<S: Into<String>>(
        &self,
        message: S,
        contexts: Vec<(String, Span)>,
    ) -> parser::AnnotatedError {
        self.0
            .as_ref()
            .error_with_contexts(message, contexts)
            .into()
    }

    pub(crate) fn ffi_error(&self, message: String) -> WrappedError {
        WrappedError(Arc::new(self.0.as_ref().error(message)))
    }

    pub(crate) fn span(&self) -> Span {
        *self.0.as_ref().span()
    }
}

#[derive(Clone, Debug)]
pub struct WrappedError(Arc<parser::Error>);

impl Custom for WrappedError {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.as_ref().to_string()))
    }
}

impl AsRef<parser::Error> for WrappedError {
    fn as_ref(&self) -> &parser::Error {
        self.0.as_ref()
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<Directive>("directive?");
    steel_engine.register_fn("transaction?", Directive::is_transaction);
    steel_engine.register_fn("price?", Directive::is_price);
    steel_engine.register_fn("balance?", Directive::is_balance);
    steel_engine.register_fn("open?", Directive::is_open);
    steel_engine.register_fn("close?", Directive::is_close);
    steel_engine.register_fn("commodity?", Directive::is_commodity);
    steel_engine.register_fn("pad?", Directive::is_pad);
    steel_engine.register_fn("document?", Directive::is_document);
    steel_engine.register_fn("note?", Directive::is_note);
    steel_engine.register_fn("event?", Directive::is_event);
    steel_engine.register_fn("query?", Directive::is_query);

    steel_engine.register_fn("transaction-postings", Directive::transaction_postings);

    steel_engine.register_type::<Posting>("posting?");
    steel_engine.register_fn("posting", Posting::create);
    steel_engine.register_fn("posting->string", Posting::to_string);
    steel_engine.register_fn("posting-account", Posting::account);
    steel_engine.register_fn("posting-amount", Posting::amount);
    steel_engine.register_fn("posting-flag", Posting::flag);

    steel_engine.register_type::<Amount>("amount?");
    steel_engine.register_fn("amount", Amount::new);
    steel_engine.register_fn("amount->string", Amount::to_string);
    steel_engine.register_fn("amount-number", Amount::number);
    steel_engine.register_fn("amount-currency", Amount::currency);

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
    steel_engine.register_fn("parse-decimal-cents", Decimal::parse_cents);

    steel_engine.register_type::<WrappedError>("error?");
    steel_engine.register_fn("ffi-error", WrappedSpannedElement::ffi_error);
}
