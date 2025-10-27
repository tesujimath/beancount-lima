// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::HashMap;
use std::{fmt::Debug, ops::Deref};

use super::{BookingError, CurrencyPosition, Inventory, Posting, Tolerance};

pub fn book<P, D, A, N, C, L, T, I>(
    date: D,
    postings: impl Iterator<Item = P>,
    tolerance: T,
    initial_inventory: I,
) -> Result<impl Iterator<Item = (A, Vec<CurrencyPosition<D, N, C, L>>)>, Vec<BookingError>>
where
    P: Posting<D, A, N, C, L>,
    D: Eq + Ord + Clone + Debug,
    A: Eq + Ord + Debug,
    N: Copy + Debug,
    C: Eq + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
    T: Tolerance<N, C>,
    I: Inventory<D, A, N, C, L, T>,
{
    let mut updated_inventory = HashMap::<A, Vec<CurrencyPosition<D, N, C, L>>>::default();

    Ok(updated_inventory.into_iter())
}
///
/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
/// 4. All positions are non-empty.
#[derive(PartialEq, Eq, Default, Debug)]
pub(crate) struct CurrencyPositions<D, N, C, L>(Vec<CurrencyPosition<D, N, C, L>>);

impl<D, N, C, L> Deref for CurrencyPositions<D, N, C, L> {
    type Target = Vec<CurrencyPosition<D, N, C, L>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
