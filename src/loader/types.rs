use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use tabulator::{Align, Cell, Gap};
use time::Date;

#[derive(Clone, Debug)]
pub(crate) struct Directive<'a> {
    pub(crate) parsed: &'a parser::Spanned<parser::Directive<'a>>,
    pub(crate) loaded: DirectiveVariant<'a>,
}

#[derive(Clone, Debug)]
pub(crate) enum DirectiveVariant<'a> {
    NA, // not applicable, as no extra data at load stage for this variant
    Transaction(Transaction<'a>),
}

#[derive(Clone, Debug)]
pub(crate) struct Transaction<'a> {
    pub(crate) postings: Vec<Posting<'a>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Posting<'a> {
    pub(crate) parsed: Option<&'a parser::Spanned<parser::Posting<'a>>>,
    pub(crate) flag: Option<String>,
    pub(crate) account: String,
    pub(crate) amount: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) cost: Option<Cost<'a>>,
    pub(crate) price: Option<Price<'a>>,
    // pub(crate) metadata: Metadata<'a>,
}

#[derive(Clone, Debug)]
pub(crate) struct Cost<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) date: Date,
    pub(crate) label: Option<&'a str>,
    pub(crate) merge: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct Price<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
}

#[derive(Clone, Debug)]
pub(crate) struct Amount<'a> {
    pub(crate) number: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
}

impl<'a> From<&'a parser::Amount<'a>> for Amount<'a> {
    fn from(value: &'a parser::Amount<'a>) -> Self {
        Amount {
            number: value.number().value(),
            currency: value.currency().item(),
        }
    }
}

impl<'a> From<Amount<'a>> for Cell<'static> {
    fn from(value: Amount) -> Self {
        Cell::Row(
            vec![
                value.number.into(),
                (value.currency.to_string(), Align::Left).into(),
            ],
            Gap::Minor,
        )
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    element_type: &'static str,
}

impl Element {
    pub(crate) fn new(element_type: &'static str, span: parser::Span) -> parser::Spanned<Self> {
        parser::spanned(Element { element_type }, span)
    }
}

impl parser::ElementType for Element {
    fn element_type(&self) -> &'static str {
        self.element_type
    }
}

pub fn into_spanned_element<T>(value: &parser::Spanned<T>) -> parser::Spanned<Element>
where
    T: parser::ElementType,
{
    parser::spanned(
        Element {
            element_type: value.element_type(),
        },
        *value.span(),
    )
}
