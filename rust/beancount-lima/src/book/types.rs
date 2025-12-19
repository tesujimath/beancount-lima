use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};
use tabulator::{Align, Cell};
use time::Date;

use crate::{
    format::{format, plain, EMPTY, GUTTER_MINOR, SPACE},
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
    pub(crate) prices: HashSet<(parser::Currency<'a>, parser::Currency<'a>, Decimal)>,
}

#[derive(Clone, Debug)]
pub(crate) struct Pad<'a> {
    pub(crate) postings: Vec<Posting<'a>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Posting<'a> {
    pub(crate) flag: Option<parser::Flag>,
    pub(crate) account: &'a str,
    pub(crate) units: Decimal,
    pub(crate) currency: parser::Currency<'a>,
    pub(crate) cost: Option<Cost<'a>>,
    pub(crate) price: Option<Price<'a>>,
    // pub(crate) metadata: Metadata<'a>,
}

pub(crate) type Cost<'a> =
    beancount_lima_booking::Cost<Date, Decimal, parser::Currency<'a>, &'a str>;

pub(crate) fn cost_to_cell<'a, 'b>(cost: &'b Cost<'a>) -> Cell<'a, 'static>
where
    'b: 'a,
{
    let mut cells = vec![
        (cost.date.to_string(), Align::Left).into(),
        cost.per_unit.into(),
        (cost.currency.as_ref(), Align::Left).into(),
    ];
    if let Some(label) = &cost.label {
        cells.push((*label, Align::Left).into())
    }
    if cost.merge {
        cells.push(("*", Align::Left).into())
    }
    Cell::Row(cells, GUTTER_MINOR)
}

pub(crate) type PostingCost<'a> = beancount_lima_booking::PostingCost<Date, Decimal, &'a str>;

pub(crate) fn cur_posting_cost_to_cost<'a>(
    currency: parser::Currency<'a>,
    cost: PostingCost<'a>,
) -> Cost<'a> {
    Cost {
        date: cost.date,
        per_unit: cost.per_unit,
        currency,
        label: cost.label,
        merge: cost.merge,
    }
}

pub(crate) type PostingCosts<'a> =
    beancount_lima_booking::PostingCosts<Date, Decimal, parser::Currency<'a>, &'a str>;

pub(crate) type Price<'a> = beancount_lima_booking::Price<Decimal, parser::Currency<'a>>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) struct Amount<'a> {
    pub(crate) number: Decimal,
    pub(crate) currency: parser::Currency<'a>,
}

impl<'a> From<(Decimal, parser::Currency<'a>)> for Amount<'a> {
    fn from(value: (Decimal, parser::Currency<'a>)) -> Self {
        Self {
            number: value.0,
            currency: value.1,
        }
    }
}

impl<'a> From<&'a parser::Amount<'a>> for Amount<'a> {
    fn from(value: &'a parser::Amount<'a>) -> Self {
        Amount {
            number: value.number().value(),
            currency: *value.currency().item(),
        }
    }
}

impl<'a> From<Amount<'a>> for Cell<'static, 'static> {
    fn from(value: Amount) -> Self {
        Cell::Row(
            vec![
                value.number.into(),
                (value.currency.to_string(), Align::Left).into(),
            ],
            GUTTER_MINOR,
        )
    }
}

impl<'a, 'b> From<&'b Amount<'a>> for Cell<'a, 'static>
where
    'b: 'a,
{
    fn from(value: &'b Amount<'a>) -> Self {
        Cell::Row(
            vec![
                value.number.into(),
                (value.currency.as_ref(), Align::Left).into(),
            ],
            GUTTER_MINOR,
        )
    }
}

pub(crate) type Positions<'a> =
    beancount_lima_booking::Positions<Date, Decimal, parser::Currency<'a>, &'a str>;

// should be From, but both types are third-party
pub(crate) fn positions_to_cell<'a, 'b>(positions: &'b Positions<'a>) -> Cell<'a, 'static>
where
    'b: 'a,
{
    Cell::Stack(positions.iter().map(position_to_cell).collect::<Vec<_>>())
}

pub(crate) type Position<'a> =
    beancount_lima_booking::Position<Date, Decimal, parser::Currency<'a>, &'a str>;

pub(crate) fn position_to_cell<'a, 'b>(position: &'b Position<'a>) -> Cell<'a, 'static>
where
    'b: 'a,
{
    let mut cells = vec![
        position.units.into(),
        (position.currency.as_ref(), Align::Left).into(),
    ];
    if let Some(cost) = &position.cost {
        cells.push(cost_to_cell(cost))
    }
    Cell::Row(cells, GUTTER_MINOR)
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
        cur: parser::Currency<'a>,
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
