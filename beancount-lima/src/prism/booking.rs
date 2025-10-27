// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use beancount_parser_lima as parser;
use std::{mem::take, ops::Deref};
use strum_macros::{Display, EnumIter, EnumString, IntoStaticStr};

use super::*;

/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
/// 4. All positions are non-empty.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct PositionsAccumulator(Vec<Position>);

impl Deref for PositionsAccumulator {
    type Target = Vec<Position>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PositionsAccumulator {
    /// Simple position accumulation.  The booking algorithm proper is in the loader
    pub(crate) fn accumulate(&mut self, position: Position) {
        // TODO handle cost
        if position.cost.is_some() {
            panic!("cost not yet supported for position accumulation")
        }

        // insert or combine with existing
        match self.0.first_mut() {
            Some(first) => {
                first.units.number += position.units.number;
            }
            None => {
                self.0.push(position);
            }
        }

        self.remove_empty_positions();
    }

    fn remove_empty_positions(&mut self) {
        // first cheaply check there are any before building a new Vec
        let any_empty = self.0.iter().any(Position::is_empty);
        if any_empty {
            let non_empty = take(&mut self.0)
                .into_iter()
                .filter_map(|p| if p.is_empty() { None } else { Some(p) })
                .collect::<Vec<_>>();
            self.0 = non_empty;
        }
    }
}

impl From<Position> for PositionsAccumulator {
    fn from(value: Position) -> Self {
        Self(vec![value])
    }
}

impl From<&PositionsAccumulator> for Vec<Position> {
    fn from(value: &PositionsAccumulator) -> Self {
        value.0.clone()
    }
}

/// The booking method for an account.
#[derive(
    EnumString, EnumIter, IntoStaticStr, PartialEq, Eq, Default, Clone, Copy, Display, Debug,
)]
#[strum(serialize_all = "kebab-case")]
pub(crate) enum Booking {
    #[default]
    Strict,
    StrictWithSize,
    None,
    Average,
    Fifo,
    Lifo,
    Hifo,
}

impl From<parser::Booking> for Booking {
    fn from(value: parser::Booking) -> Self {
        use Booking::*;

        match value {
            parser::Booking::Strict => Strict,
            parser::Booking::StrictWithSize => StrictWithSize,
            parser::Booking::None => None,
            parser::Booking::Average => Average,
            parser::Booking::Fifo => Fifo,
            parser::Booking::Lifo => Lifo,
            parser::Booking::Hifo => Hifo,
        }
    }
}

impl From<Booking> for parser::Booking {
    fn from(value: Booking) -> Self {
        use Booking::*;

        match value {
            Strict => parser::Booking::Strict,
            StrictWithSize => parser::Booking::StrictWithSize,
            None => parser::Booking::None,
            Average => parser::Booking::Average,
            Fifo => parser::Booking::Fifo,
            Lifo => parser::Booking::Lifo,
            Hifo => parser::Booking::Hifo,
        }
    }
}
