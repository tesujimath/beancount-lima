// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser, ElementType, Span, Spanned};
use color_eyre::eyre::Result;
use std::{fmt::Display, ops::Deref, sync::Arc};
use steel::{
    rvals::{as_underlying_type, Custom, CustomType, SteelString, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr, SteelVal,
};
use steel_derive::Steel;

use crate::{steel_date::SteelDate, steel_decimal::SteelDecimal};

#[derive(Clone, Debug)]
pub(crate) struct Directive {
    pub(crate) date: SteelDate,
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
    pub(crate) tolerance: Option<SteelDecimal>,
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
    pub(crate) number: SteelDecimal,
    pub(crate) currency: SteelString,
}

// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn new(number: SteelDecimal, currency: SteelString) -> Self {
        Self { number, currency }
    }

    fn number(&self) -> SteelDecimal {
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

#[derive(Clone, Debug)]
pub(crate) struct CustomShared<T>(Arc<T>);

impl<T> From<T> for CustomShared<T> {
    fn from(value: T) -> Self {
        CustomShared(Arc::new(value))
    }
}

impl<T> Deref for CustomShared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl<T> Custom for CustomShared<T> where T: 'static {}

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
pub struct WrappedSpannedElement(CustomShared<Spanned<Element>>);

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
        WrappedSpannedElement(
            parser::spanned(
                Element {
                    element_type: spanned_element.element_type(),
                },
                *spanned_element.span(),
            )
            .into(),
        )
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
        WrappedError(self.0.as_ref().error(message).into())
    }

    pub(crate) fn span(&self) -> Span {
        *self.0.as_ref().span()
    }
}

#[derive(Clone, Debug)]
pub struct WrappedError(CustomShared<parser::Error>);

impl Custom for WrappedError {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }
}

impl AsRef<parser::Error> for WrappedError {
    fn as_ref(&self) -> &parser::Error {
        &self.0
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
    steel_engine.register_fn("posting->string", Posting::to_string);
    steel_engine.register_fn("posting-account", Posting::account);
    steel_engine.register_fn("posting-amount", Posting::amount);
    steel_engine.register_fn("posting-flag", Posting::flag);

    steel_engine.register_type::<Amount>("amount?");
    steel_engine.register_fn("amount", Amount::new);
    steel_engine.register_fn("amount->string", Amount::to_string);
    steel_engine.register_fn("amount-number", Amount::number);
    steel_engine.register_fn("amount-currency", Amount::currency);

    steel_engine.register_type::<WrappedError>("error?");
    steel_engine.register_fn("ffi-error", WrappedSpannedElement::ffi_error);
}
