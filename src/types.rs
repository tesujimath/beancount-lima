// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser, ElementType, Span, Spanned};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::{fmt::Display, ops::Deref};
use steel::{
    gc::Shared,
    rvals::{as_underlying_type, Custom, CustomType, SteelString, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr, SteelVal,
};
use steel_derive::Steel;

use crate::{steel_date::SteelDate, steel_decimal::SteelDecimal};

#[derive(Clone, Debug)]
pub(crate) struct SteelDirective {
    pub(crate) date: SteelDate,
    pub(crate) element: WrappedSpannedElement,
    pub(crate) variant: DirectiveVariant,
}

impl Custom for SteelDirective {
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
impl SteelDirective {
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
    pub(crate) amount: SteelAmount,
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
pub(crate) struct SteelPosting {
    pub(crate) flag: Option<SteelString>,
    pub(crate) account: SteelString,
    pub(crate) amount: SteelAmount,
    // TODO:
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl SteelPosting {
    pub(crate) fn new<S1, S2>(account: S1, amount: SteelAmount, flag: Option<S2>) -> Self
    where
        S1: Display,
        S2: Display,
    {
        SteelPosting {
            account: account.to_string().into(),
            amount,
            flag: flag.map(|flag| flag.to_string().into()),
        }
    }

    fn account(&self) -> SteelString {
        self.account.clone()
    }

    fn amount(&self) -> SteelAmount {
        self.amount.clone()
    }

    fn flag(&self) -> Option<SteelString> {
        self.flag.clone()
    }
}

impl Custom for SteelPosting {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelPosting>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SteelAmount {
    pub(crate) number: SteelDecimal,
    pub(crate) currency: SteelString,
}

// https://github.com/mattwparas/steel/issues/365
impl SteelAmount {
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

impl Display for SteelAmount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.number, &self.currency)
    }
}

impl Custom for SteelAmount {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelAmount>(other) {
            self == other
        } else {
            false
        }
    }
}

impl<S> From<(Decimal, S)> for SteelAmount
where
    S: Display,
{
    fn from(value: (Decimal, S)) -> Self {
        SteelAmount {
            number: value.0.into(),
            currency: value.1.to_string().into(),
        }
    }
}

impl From<&parser::Amount<'_>> for SteelAmount {
    fn from(value: &parser::Amount) -> Self {
        (value.number().value(), value.currency()).into()
    }
}

/// For third-party types we need to share
#[derive(Clone, Debug)]
pub(crate) struct CustomShared<T>(Shared<T>);

impl<T> From<T> for CustomShared<T> {
    fn from(value: T) -> Self {
        CustomShared(Shared::new(value))
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
    steel_engine.register_type::<SteelDirective>("directive?");
    steel_engine.register_fn("transaction?", SteelDirective::is_transaction);
    steel_engine.register_fn("price?", SteelDirective::is_price);
    steel_engine.register_fn("balance?", SteelDirective::is_balance);
    steel_engine.register_fn("open?", SteelDirective::is_open);
    steel_engine.register_fn("close?", SteelDirective::is_close);
    steel_engine.register_fn("commodity?", SteelDirective::is_commodity);
    steel_engine.register_fn("pad?", SteelDirective::is_pad);
    steel_engine.register_fn("document?", SteelDirective::is_document);
    steel_engine.register_fn("note?", SteelDirective::is_note);
    steel_engine.register_fn("event?", SteelDirective::is_event);
    steel_engine.register_fn("query?", SteelDirective::is_query);

    steel_engine.register_fn("transaction-postings", SteelDirective::transaction_postings);

    steel_engine.register_type::<SteelPosting>("posting?");
    steel_engine.register_fn("posting->string", SteelPosting::to_string);
    steel_engine.register_fn("posting-account", SteelPosting::account);
    steel_engine.register_fn("posting-amount", SteelPosting::amount);
    steel_engine.register_fn("posting-flag", SteelPosting::flag);

    steel_engine.register_type::<SteelAmount>("amount?");
    steel_engine.register_fn("amount", SteelAmount::new);
    steel_engine.register_fn("amount->string", SteelAmount::to_string);
    steel_engine.register_fn("amount-number", SteelAmount::number);
    steel_engine.register_fn("amount-currency", SteelAmount::currency);

    steel_engine.register_type::<WrappedError>("error?");
    steel_engine.register_fn("ffi-error", WrappedSpannedElement::ffi_error);
}
