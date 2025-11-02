use hashbrown::HashMap;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Deref, Div, Mul, Neg},
};
use strum_macros::Display;

pub trait Posting: Clone {
    type Date: Eq + Ord + Copy + Display + Debug;
    type Account: Eq + Hash + Clone + Display + Debug;
    type Currency: Eq + Hash + Ord + Clone + Display + Debug;
    type Number: Number + Eq + Copy + Display + Debug;
    type CostSpec: CostSpec<
            Date = Self::Date,
            Currency = Self::Currency,
            Number = Self::Number,
            Label = Self::Label,
        > + Clone
        + Display
        + Debug;
    type PriceSpec: PriceSpec<Currency = Self::Currency, Number = Self::Number>
        + Clone
        + Display
        + Debug;
    type Label: Eq + Ord + Clone + Display + Debug;

    fn account(&self) -> Self::Account;
    fn currency(&self) -> Option<Self::Currency>;
    fn units(&self) -> Option<Self::Number>;
    fn cost(&self) -> Option<Self::CostSpec>;
    fn price(&self) -> Option<Self::PriceSpec>;
}

pub trait CostSpec: Clone {
    type Date: Eq + Ord + Copy + Display + Debug;
    type Currency: Eq + Hash + Ord + Clone + Display + Debug;
    type Number: Number + Eq + Copy + Display + Debug;
    type Label: Eq + Ord + Clone + Display + Debug;

    fn currency(&self) -> Option<Self::Currency>;
    fn per_unit(&self) -> Option<Self::Number>;
    fn total(&self) -> Option<Self::Number>;
    fn date(&self) -> Option<Self::Date>;
    fn label(&self) -> Option<Self::Label>;
    fn merge(&self) -> bool;
}

pub trait PriceSpec: Clone {
    type Currency: Eq + Hash + Ord + Clone + Display + Debug;
    type Number: Number + Eq + Copy + Display + Debug;

    fn currency(&self) -> Option<Self::Currency>;
    fn per_unit(&self) -> Option<Self::Number>;
    fn total(&self) -> Option<Self::Number>;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Position<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub currency: C,
    pub units: N,
    pub cost: Option<Cost<D, N, C, L>>,
}

impl<D, N, C, L> Position<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub(crate) fn with_accumulated(&self, units: N) -> Self
    where
        C: Clone,
        N: Add<Output = N> + Copy,
    {
        let cost = self.cost.as_ref().cloned();
        Position {
            currency: self.currency.clone(),
            units: self.units + units,
            cost,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Cost<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub date: D,
    pub per_unit: N,
    pub currency: C,
    pub label: Option<L>,
    pub merge: bool,
}

impl<D, N, C, L> Cost<D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub(crate) fn matches_spec<CS>(&self, cost_spec: &CS, default_date: D) -> bool
    where
        D: Eq,
        N: Eq,
        C: Eq,
        L: Eq,
        CS: CostSpec<Date = D, Number = N, Currency = C, Label = L>,
    {
        !(
            cost_spec.date().unwrap_or(default_date) != self.date
                || cost_spec
                    .currency()
                    .is_some_and(|cost_spec_currency| cost_spec_currency != self.currency)
                || cost_spec
                    .per_unit()
                    .is_some_and(|cost_spec_units| cost_spec_units != self.per_unit)
                || cost_spec
                    .currency()
                    .is_some_and(|cost_spec_currency| cost_spec_currency != self.currency)
                || cost_spec.label().is_some_and(|cost_spec_label| {
                    self.label
                        .as_ref()
                        .is_some_and(|cost_label| *cost_label != cost_spec_label)
                })
            // TODO merge
        )
    }
}

impl<D, N, C, L> PartialOrd for Cost<D, N, C, L>
where
    D: Ord + Copy,
    N: Ord + Copy,
    C: Ord + Clone,
    L: Ord + Clone,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<D, N, C, L> Ord for Cost<D, N, C, L>
where
    D: Ord + Copy,
    N: Ord + Copy,
    C: Ord + Clone,
    L: Ord + Clone,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;

        match self.date.cmp(&other.date) {
            Equal => {}
            ord => return ord,
        }
        match self.currency.cmp(&other.currency) {
            Equal => {}
            ord => return ord,
        }
        match self.label.cmp(&other.label) {
            Equal => {}
            ord => return ord,
        }
        match self.merge.cmp(&other.merge) {
            Equal => {}
            ord => return ord,
        }

        self.per_unit.cmp(&other.per_unit)
    }
}

pub trait Tolerance {
    type Currency;
    type Number;

    /// compute residual, ignoring sums which are tolerably small
    fn residual(
        &self,
        values: impl Iterator<Item = Self::Number>,
        cur: &Self::Currency,
    ) -> Option<Self::Number>;
}

pub trait Number:
    Add<Output = Self>
    + AddAssign
    + Neg<Output = Self>
    + Mul<Output = Self>
    + Div<Output = Self>
    + Sum
    + Ord
    + Sized
{
    fn abs(&self) -> Self;

    // zero is neither positive nor negative
    fn sign(&self) -> Option<Sign>;

    fn zero() -> Self;
}

#[derive(PartialEq, Eq, Clone, Copy, Display, Debug)]
pub enum Sign {
    Positive,
    Negative,
}

/// The booking method for an account.
#[derive(PartialEq, Eq, Default, Clone, Copy, Display, Debug)]
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

#[derive(Debug)]
pub struct UpdatedInventory<P>
where
    P: Posting,
{
    value: HashMap<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>,
}

impl<P> Default for UpdatedInventory<P>
where
    P: Posting,
{
    fn default() -> Self {
        Self {
            value: Default::default(),
        }
    }
}

impl<P> From<HashMap<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>>
    for UpdatedInventory<P>
where
    P: Posting,
{
    fn from(
        value: HashMap<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>,
    ) -> Self {
        Self { value }
    }
}

impl<P> Deref for UpdatedInventory<P>
where
    P: Posting,
{
    type Target = HashMap<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<P> IntoIterator for UpdatedInventory<P>
where
    P: Posting,
{
    type Item = (
        P::Account,
        Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
    );
    type IntoIter = hashbrown::hash_map::IntoIter<
        P::Account,
        Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
    >;

    fn into_iter(
        self,
    ) -> hashbrown::hash_map::IntoIter<
        P::Account,
        Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
    > {
        self.value.into_iter()
    }
}

impl<P> UpdatedInventory<P>
where
    P: Posting,
{
    pub(crate) fn insert(
        &mut self,
        k: P::Account,
        v: Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
    ) -> Option<Vec<Position<P::Date, P::Number, P::Currency, P::Label>>> {
        self.value.insert(k, v)
    }
}
