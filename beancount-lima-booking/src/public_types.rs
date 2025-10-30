use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter::Sum,
    ops::{Add, Mul, Neg},
};
use strum_macros::Display;

pub trait Posting: Clone {
    type Date: Eq + Ord + Copy + Debug;
    type Account: Eq + Hash + Clone + Display + Debug;
    type Currency: Eq + Hash + Ord + Clone + Debug;
    type Number: Number + Eq + Copy + Display + Debug;
    type Label: Eq + Ord + Clone + Debug;

    fn account(&self) -> Self::Account;

    fn currency(&self) -> Option<Self::Currency>;
    fn units(&self) -> Option<Self::Number>;

    // TODO remove these in favour of matches cost?
    fn has_cost(&self) -> bool;
    fn cost_currency(&self) -> Option<Self::Currency>;
    fn cost_per_unit(&self) -> Option<Self::Number>;
    fn cost_total(&self) -> Option<Self::Number>;
    fn cost_date(&self) -> Option<Self::Date>;
    fn cost_label(&self) -> Option<Self::Label>;
    fn cost_merge(&self) -> Option<bool>;
    fn matches_cost(
        &self,
        default_date: Self::Date,
        cost: &Cost<Self::Date, Self::Number, Self::Currency, Self::Label>,
    ) -> bool;

    fn has_price(&self) -> bool;
    fn price_currency(&self) -> Option<Self::Currency>;
    fn price_per_unit(&self) -> Option<Self::Number>;
    fn price_total(&self) -> Option<Self::Number>;
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
    Add<Output = Self> + Neg<Output = Self> + Mul<Output = Self> + Sum + Sized
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
