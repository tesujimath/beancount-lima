// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima as parser;
use std::ops::Deref;
use strum_macros::{Display, EnumIter, EnumString, IntoStaticStr};

use crate::types::*;

/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct PositionsBuilder(Vec<Position>);

impl Deref for PositionsBuilder {
    type Target = Vec<Position>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PositionsBuilder {
    /// The booking algorithm, TODO cost
    pub(crate) fn book(&mut self, position: Position, method: Booking) {
        if position.cost.is_some() {
            panic!("cost not yet supported for booking")
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
    }

    // return non-zero positions only
    pub(crate) fn into_positions(self) -> impl Iterator<Item = Position> {
        self.0
            .into_iter()
            .filter(|p| !p.units.number.is_zero() || p.cost.is_some())
    }
}

impl From<Position> for PositionsBuilder {
    fn from(value: Position) -> Self {
        Self(vec![value])
    }
}

/// The booking method for an account.
#[derive(
    EnumString, EnumIter, IntoStaticStr, PartialEq, Eq, Default, Clone, Copy, Display, Debug,
)]
#[strum(serialize_all = "kebab-case")]
pub enum Booking {
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
