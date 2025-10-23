use std::{cmp::Ordering, fmt, mem::take, ops::Deref};

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
    pub(crate) fn book(
        &mut self,
        date: Date,
        units: Decimal,
        cost_spec: Option<&'a parser::CostSpec<'a>>,
        method: parser::Booking,
    ) -> Result<Option<Cost<'a>>, &'static str> {
        if method != parser::Booking::Strict {
            panic!("booking method {method} not yet implemented, please use strict for now");
        }

        let cost = if let Some(cost_spec) = cost_spec {
            use Operation::*;

            match self.operation(units) {
                Augmentation => Some(self.augment(date, units, cost_spec, method)?),
                Reduction => Some(self.reduce(date, units, cost_spec, method)?),
            }
        } else {
            self.book_simple(units);
            None
        };

        self.remove_empty_positions();

        Ok(cost)
    }

    pub(crate) fn augment(
        &mut self,
        date: Date,
        units: Decimal,
        cost_spec: &'a parser::CostSpec<'a>,
        method: parser::Booking,
    ) -> Result<Cost<'a>, &'static str> {
        let date = cost_spec.date().map(|date| *date.item()).unwrap_or(date);
        let per_unit = cost_spec
            .per_unit()
            .map(|per_unit| per_unit.item().value())
            .or(cost_spec.total().map(|total| total.item().value() / units))
            .ok_or("cost per-unit or total must be specified in augmentation")?;
        let currency = cost_spec
            .currency()
            .map(|cur| cur.item())
            .ok_or("cost currency must be specified in augmentation")?;
        let label = cost_spec.label().map(|label| *label.item());
        let merge = cost_spec.merge();

        let cost = Cost {
            per_unit,
            currency,
            date,
            label,
            merge,
        };

        self.insert_or_combine(units, cost.clone());

        Ok(cost)
    }

    pub(crate) fn reduce(
        &mut self,
        date: Date,
        units: Decimal,
        cost_spec: &'a parser::CostSpec<'a>,
        method: parser::Booking,
    ) -> Result<Cost<'a>, &'static str> {
        // TODO this should have better cost matching, but for now we replicate the augment behaviour
        let date = cost_spec.date().map(|date| *date.item()).unwrap_or(date);
        let per_unit = cost_spec
            .per_unit()
            .map(|per_unit| per_unit.item().value())
            .or(cost_spec.total().map(|total| total.item().value() / units))
            .ok_or("cost per-unit or total must be specified in reduction")?;
        let currency = cost_spec
            .currency()
            .map(|cur| cur.item())
            .ok_or("cost currency must be specified in reduction")?;
        let label = cost_spec.label().map(|label| *label.item());
        let merge = cost_spec.merge();

        let cost = Cost {
            per_unit,
            currency,
            date,
            label,
            merge,
        };

        self.insert_or_combine(units, cost.clone());

        Ok(cost)
    }

    fn insert_or_combine(&mut self, units: Decimal, cost: Cost<'a>) {
        use Ordering::*;

        match self.binary_search_by(|position| match &position.cost {
            None => Less,
            Some(position_cost) => position_cost.partial_cmp(&cost).unwrap_or(Equal),
        }) {
            Ok(i) => {
                let position = &mut self.0[i];
                tracing::debug!(
                    "augmenting position {:?} with {} {:?}",
                    &position,
                    units,
                    &cost
                );
                position.units += units;
            }
            Err(i) => {
                let position = CurrencyPosition {
                    units,
                    cost: Some(cost),
                };
                tracing::debug!("inserting new position {:?} at {i}", &position);
                self.0.insert(i, position)
            }
        }
    }

    /// determine whether accumulating the new cost position will be an augmentation or a reduction
    fn operation(&self, units: Decimal) -> Operation {
        if let Some(direction) = self.cost_direction() {
            direction.operation(units)
        } else {
            Operation::Augmentation
        }
    }

    /// determine the direction of our cost position, i.e. Short, Long, Mixed, or None if there are no costs
    fn cost_direction(&self) -> Option<CostDirection> {
        let mut direction = None;
        for position in &self.0 {
            if position.cost.is_some() {
                let position_direction = position.units.into();

                if let Some(direction) = direction {
                    if direction != position_direction {
                        return Some(CostDirection::Mixed);
                    }
                } else {
                    direction = Some(position_direction);
                }
            }
        }

        direction
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

    pub(crate) fn format(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        cur: &'a parser::Currency<'a>,
    ) -> fmt::Result {
        format(f, self.0.iter(), |f1, p| p.format(f1, cur), ", ", None)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum CostDirection {
    Long,
    Short,
    Mixed,
}

impl CostDirection {
    fn operation(self, units: Decimal) -> Operation {
        use CostDirection::*;

        if self == Long && units.is_sign_negative() || self == Short && units.is_sign_positive() {
            Operation::Reduction
        } else {
            Operation::Augmentation
        }
    }
}

impl From<Decimal> for CostDirection {
    fn from(value: Decimal) -> Self {
        if value.is_sign_negative() {
            CostDirection::Short
        } else {
            CostDirection::Long
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Operation {
    Augmentation,
    Reduction,
}
