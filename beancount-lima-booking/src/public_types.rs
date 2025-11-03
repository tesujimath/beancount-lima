use hashbrown::HashMap;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Deref, Div, Mul, Neg},
};
use strum_macros::Display;

pub trait PostingSpec: Clone {
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

pub trait Posting: Clone {
    type Date: Eq + Ord + Copy + Display + Debug;
    type Account: Eq + Hash + Clone + Display + Debug;
    type Currency: Eq + Hash + Ord + Clone + Display + Debug;
    type Number: Number + Eq + Copy + Display + Debug;
    type Label: Eq + Ord + Clone + Display + Debug;

    fn account(&self) -> Self::Account;
    fn currency(&self) -> Self::Currency;
    fn units(&self) -> Self::Number;
    fn cost(
        &self,
    ) -> Option<impl Iterator<Item = Cost<Self::Date, Self::Number, Self::Currency, Self::Label>>>;
    fn price(&self) -> Option<Price<Self::Number, Self::Currency>>;
}

pub trait CostSpec: Clone {
    type Date: Eq + Ord + Copy + Display + Debug;
    type Currency: Eq + Hash + Ord + Clone + Display + Debug;
    type Number: Number + Eq + Copy + Display + Debug;
    type Label: Eq + Ord + Clone + Display + Debug;

    fn date(&self) -> Option<Self::Date>;
    fn per_unit(&self) -> Option<Self::Number>;
    fn total(&self) -> Option<Self::Number>;
    fn currency(&self) -> Option<Self::Currency>;
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Price<N, C>
where
    N: Copy,
    C: Clone,
{
    pub per_unit: N,
    pub currency: C,
}

#[derive(Clone, Debug)]
pub struct Interpolated<P, D, N, C, L>
where
    D: Copy,
    N: Copy,
    C: Clone,
    L: Clone,
{
    pub posting: P,
    pub units: N,
    pub currency: C,
    pub cost: Option<Cost<D, N, C, L>>,
    pub price: Option<Price<N, C>>,
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
    + Eq
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
pub struct UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    value: HashMap<A, Vec<Position<D, N, C, L>>>,
}

impl<A, D, N, C, L> Default for UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    fn default() -> Self {
        Self {
            value: Default::default(),
        }
    }
}

impl<A, D, N, C, L> From<HashMap<A, Vec<Position<D, N, C, L>>>> for UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    fn from(value: HashMap<A, Vec<Position<D, N, C, L>>>) -> Self {
        Self { value }
    }
}

impl<A, D, N, C, L> Deref for UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    type Target = HashMap<A, Vec<Position<D, N, C, L>>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<A, D, N, C, L> IntoIterator for UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    type Item = (A, Vec<Position<D, N, C, L>>);
    type IntoIter = hashbrown::hash_map::IntoIter<A, Vec<Position<D, N, C, L>>>;

    fn into_iter(self) -> hashbrown::hash_map::IntoIter<A, Vec<Position<D, N, C, L>>> {
        self.value.into_iter()
    }
}

impl<A, D, N, C, L> UpdatedInventory<A, D, N, C, L>
where
    A: Eq + Hash + Clone + Display + Debug,
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    pub(crate) fn insert(
        &mut self,
        k: A,
        v: Vec<Position<D, N, C, L>>,
    ) -> Option<Vec<Position<D, N, C, L>>> {
        self.value.insert(k, v)
    }
}
