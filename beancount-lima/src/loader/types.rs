use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use std::{
    collections::HashMap,
    fmt::{self, Display},
};
use tabulator::{Align, Cell, Gap};
use time::Date;

use crate::{
    format::{format, plain, EMPTY, SPACE},
    options::defaults::default_inferred_tolerance_multiplier,
};

#[derive(Clone, Debug)]
pub(crate) struct Directive<'a> {
    pub(crate) parsed: &'a parser::Spanned<parser::Directive<'a>>,
    pub(crate) loaded: DirectiveVariant<'a>,
}

#[derive(Clone, Debug)]
pub(crate) enum DirectiveVariant<'a> {
    NA, // not applicable, as no extra data at load stage for this variant
    Transaction(Transaction<'a>),
    Pad(Pad<'a>),
}

#[derive(Clone, Debug)]
pub(crate) struct Transaction<'a> {
    pub(crate) postings: Vec<Posting<'a>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Pad<'a> {
    pub(crate) postings: Vec<Posting<'a>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Posting<'a> {
    pub(crate) parsed: Option<&'a parser::Spanned<parser::Posting<'a>>>,
    pub(crate) flag: Option<parser::Flag>,
    pub(crate) account: &'a str,
    pub(crate) units: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    // pub(crate) cost: Option<
    //     beancount_lima_booking::PostingCosts<Date, Decimal, &'a parser::Currency<'a>, &'a str>,
    // >,
    // TODO price
    // pub(crate) price: Option<Price<'a>>,
    // pub(crate) metadata: Metadata<'a>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) struct Cost<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) date: Date,
    pub(crate) label: Option<&'a str>,
    pub(crate) merge: bool,
}

impl<'a> Display for Cost<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}, {} {}", &self.date, &self.per_unit, &self.currency)?;

        if let Some(label) = &self.label {
            write!(f, ", \"{label}\"")?;
        }

        if self.merge {
            write!(f, ", *",)?;
        }

        f.write_str("}")
    }
}

// costs with all the same non-numeric fields are equal if the per-unit is
// equal, otherwise incomparable
impl<'a> PartialOrd for Cost<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.date.partial_cmp(&other.date) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.currency.partial_cmp(other.currency) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.label.partial_cmp(&other.label) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.merge.partial_cmp(&other.merge) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }

        match self.per_unit.partial_cmp(&other.per_unit) {
            equal @ Some(core::cmp::Ordering::Equal) => equal,
            ord => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) struct Price<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
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

#[derive(PartialEq, Eq, Clone, Debug)]
/// CurrencyPosition for implicit currency, which is kept externally
pub(crate) struct CurrencyPosition<'a> {
    pub(crate) units: Decimal,
    pub(crate) cost: Option<Cost<'a>>,
}

impl<'a> CurrencyPosition<'a> {
    pub(crate) fn is_empty(&self) -> bool {
        // TODO do we need a tolerance check here?
        self.units.is_zero() && self.cost.is_none()
    }

    pub(crate) fn format(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        cur: &'a parser::Currency<'a>,
    ) -> fmt::Result {
        write!(f, "{} {}", self.units, cur)?;
        format(f, &self.cost, plain, EMPTY, Some(SPACE))
    }
}

#[derive(Debug)]
pub(crate) struct InferredTolerance<'a> {
    pub(crate) fallback: Option<Decimal>,
    pub(crate) by_currency: HashMap<parser::Currency<'a>, Decimal>,

    pub(crate) multiplier: Decimal,
}

impl<'a> InferredTolerance<'a> {
    pub(crate) fn new(options: &'a parser::Options<'a>) -> Self {
        Self {
            fallback: options.inferred_tolerance_default_fallback(),
            by_currency: options
                .inferred_tolerance_defaults()
                .filter_map(|(cur, value)| cur.map(|cur| (cur, value)))
                .collect::<HashMap<_, _>>(),
            multiplier: options
                .inferred_tolerance_multiplier()
                .map(|m| *m.item())
                .unwrap_or(default_inferred_tolerance_multiplier()),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Element {
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

pub(crate) fn into_spanned_element<T>(value: &parser::Spanned<T>) -> parser::Spanned<Element>
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

#[cfg(test)]
mod tests;
