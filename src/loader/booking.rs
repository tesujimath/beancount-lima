use std::{fmt, mem::take, ops::Deref};

use super::*;
use crate::format::format;

/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
/// 4. All positions are non-empty.
#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub(crate) struct CurrencyPositionsBuilder<'a>(Vec<CurrencyPosition<'a>>);

impl<'a> Deref for CurrencyPositionsBuilder<'a> {
    type Target = Vec<CurrencyPosition<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> CurrencyPositionsBuilder<'a> {
    pub(crate) fn format(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        cur: &'a parser::Currency<'a>,
    ) -> fmt::Result {
        format(f, self.0.iter(), |f1, p| p.format(f1, cur), ", ", None)
    }
}

fn initial_cost<'a>(
    date: Date,
    weight: &Weight<'a>,
    cost_spec: &'a parser::CostSpec<'a>,
) -> Cost<'a> {
    let native_units = if let WeightSource::Cost(native_units, _) = &weight.source {
        native_units
    } else {
        panic!(
            "impossible weight source {:?} for posting with cost",
            &weight.source
        );
    };
    let per_unit = weight.number / native_units;
    let currency = weight.currency;
    let date = cost_spec.date().map(|date| *date.item()).unwrap_or(date);
    let label = cost_spec.label().map(|label| *label.item());
    let merge = cost_spec.merge();

    Cost {
        per_unit,
        currency,
        date,
        label,
        merge,
    }
}

impl<'a> CurrencyPositionsBuilder<'a> {
    pub(crate) fn units(&self) -> Decimal {
        self.iter().map(|p| p.units).sum()
    }

    /// initial position
    pub(crate) fn new(
        date: Date,
        weight: &Weight<'a>,
        cost_spec: Option<&'a parser::CostSpec<'a>>,
    ) -> Self {
        use WeightSource::*;

        let (units, cost) = match (&weight.source, cost_spec) {
            (Native, None) => (weight.number, None),

            (Cost(_, _), Some(cost_spec)) => {
                initial_cost(date, weight, cost_spec);
                panic!("cost not yet supported for booking");
            }

            (Price(native_units, _), None) => (*native_units, None),

            (source, cost_spec) => {
                panic!("inconsistent weight source {source:?}  and cost_spec {cost_spec:?}")
            }
        };
        Self(vec![CurrencyPosition { units, cost }])
    }

    /// The booking algorithm
    // TODO remove equivalent from prism
    pub(crate) fn book(
        &mut self,
        date: Date,
        amount: &Amount<'a>,
        cost_spec: Option<&'a parser::CostSpec<'a>>,
        method: parser::Booking,
    ) {
        if method != parser::Booking::Strict {
            panic!("booking method {method} not yet implemented");
        }
        if cost_spec.is_some() {
            panic!("cost not yet supported for booking");
        }

        // TODO book with costs
        self.book_simple(amount.number);

        self.remove_empty_positions();
    }

    fn book_simple(&mut self, units: Decimal) {
        // insert or combine with existing
        match self.0.first_mut() {
            Some(first) => {
                first.units += units;
            }
            None => {
                self.0.push(CurrencyPosition { units, cost: None });
            }
        }
    }

    /// make a pseudo-booking to adjust the balance, for better error reporting of balance violations
    pub(crate) fn adjust_for_better_balance_violation_reporting(&mut self, correction: Decimal) {
        self.book_simple(correction);

        self.remove_empty_positions();
    }

    fn remove_empty_positions(&mut self) {
        // first cheaply check there are any before building a new Vec
        let any_empty = self.0.iter().any(CurrencyPosition::is_empty);
        if any_empty {
            let non_empty = take(&mut self.0)
                .into_iter()
                .filter_map(|p| if p.is_empty() { None } else { Some(p) })
                .collect::<Vec<_>>();
            self.0 = non_empty;
        }
    }
}
