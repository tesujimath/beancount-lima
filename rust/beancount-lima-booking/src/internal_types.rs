// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{hash_map::Entry, HashMap};
use std::{fmt::Debug, hash::Hash, ops::Deref};

use super::{Cost, CostSpec, Interpolated, Number, PostingSpec, PriceSpec};

///
/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
/// 4. All positions are non-empty.
#[derive(PartialEq, Eq, Default, Debug)]
pub(crate) struct CurrencyPositions<D, N, C, L>(Vec<CurrencyPosition<D, N, C, L>>)
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone;

impl<D, N, C, L> Deref for CurrencyPositions<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    type Target = Vec<CurrencyPosition<D, N, C, L>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// CurrencyPosition for implicit currency, which is kept externally
pub(crate) struct CurrencyPosition<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    units: N,
    cost: Option<Cost<D, N, C, L>>,
}

impl<D, N, C, L> CurrencyPosition<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub(crate) fn is_below(&self, threshold: N) -> bool
    where
        N: Number + Ord,
    {
        // TODO ensure that costs are not left below threshold
        self.units.abs() <= threshold && self.cost.is_none()
    }
}

#[derive(Debug)]
pub(crate) struct HashMapOfVec<K, V>(HashMap<K, Vec<V>>);

impl<K, V> HashMapOfVec<K, V> {
    pub(crate) fn push_or_insert(&mut self, k: K, v: V)
    where
        K: Eq + Hash,
    {
        use Entry::*;

        match self.0.entry(k) {
            Occupied(mut occupied) => {
                occupied.get_mut().push(v);
            }
            Vacant(vacant) => {
                vacant.insert(vec![v]);
            }
        }
    }
}

impl<K, V> Default for HashMapOfVec<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K, V> IntoIterator for HashMapOfVec<K, V> {
    type Item = (K, Vec<V>);
    type IntoIter = hashbrown::hash_map::IntoIter<K, Vec<V>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K, V> Deref for HashMapOfVec<K, V> {
    type Target = HashMap<K, Vec<V>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AnnotatedPosting<P, C>
where
    C: Clone,
{
    pub(crate) posting: P,
    pub(crate) idx: usize,
    pub(crate) currency: Option<C>,
    pub(crate) cost_currency: Option<C>,
    pub(crate) price_currency: Option<C>,
}

impl<P, C> AnnotatedPosting<P, C>
where
    P: PostingSpec,
    C: Clone,
{
    // the bucket is the currency used for balancing weights during inference, not the currency booked to
    pub(crate) fn bucket(&self) -> Option<C>
    where
        C: Clone,
    {
        self.cost_currency
            .as_ref()
            .cloned()
            .or(self.price_currency.as_ref().cloned())
            .or_else(|| {
                // use the posting currency as the bucket only if there's neither cost nor price
                if self.posting.cost().is_none() && self.posting.price().is_none() {
                    self.currency.as_ref().cloned()
                } else {
                    None
                }
            })
    }
}

#[derive(Clone, Debug)]
pub(crate) enum BookedOrUnbookedPosting<P>
where
    P: PostingSpec,
{
    Booked(Interpolated<P, P::Date, P::Number, P::Currency, P::Label>),
    Unbooked(AnnotatedPosting<P, P::Currency>),
}

impl<P> BookedOrUnbookedPosting<P>
where
    P: PostingSpec,
{
    // determine the weight of a posting
    // https://beancount.github.io/docs/beancount_language_syntax.html#balancing-rule-the-weight-of-postings
    pub(crate) fn weight(&self) -> Option<P::Number> {
        use BookedOrUnbookedPosting::*;

        match self {
            Booked(booked) => Some(booked.units),
            Unbooked(unbooked) => {
                let p = &unbooked.posting;

                if let Some(cost_spec) = p.cost() {
                    match (cost_spec.total(), cost_spec.per_unit(), p.units()) {
                        (Some(cost_total), _, _) => Some(cost_total),
                        (None, Some(cost_per_unit), Some(units)) => {
                            let weight = (cost_per_unit * units).rescaled(units.scale());
                            Some(weight)
                        }
                        _ => None,
                    }
                } else if let Some(price_spec) = p.price() {
                    match (price_spec.total(), price_spec.per_unit(), p.units()) {
                        (Some(price_total), _, _) => Some(price_total),
                        (None, Some(price_per_unit), Some(units)) => {
                            let weight = (price_per_unit * units).rescaled(units.scale());
                            tracing::debug!("weight {weight} from price_per_unit {price_per_unit} units {units}");
                            Some(weight)
                        }
                        _ => None,
                    }
                } else {
                    p.units()
                }
            }
        }
    }
}
