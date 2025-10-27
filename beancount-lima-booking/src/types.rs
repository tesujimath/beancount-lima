use std::{
    ops::{Add, Sub},
    {fmt::Debug, hash::Hash},
};

pub trait Posting {
    type Date: Eq + Ord + Clone + Debug;
    type Account: Eq + Hash + Clone + Debug;
    type Currency: Eq + Hash + Ord + Clone + Debug;
    type Number: Copy + Debug;
    type Label: Eq + Ord + Clone + Debug;

    fn account(&self) -> Self::Account;

    fn currency(&self) -> Option<Self::Currency>;
    fn units(&self) -> Option<Self::Number>;

    fn has_cost(&self) -> bool;
    fn cost_currency(&self) -> Option<Self::Currency>;
    fn cost_per_unit(&self) -> Option<Self::Number>;
    fn cost_total(&self) -> Option<Self::Number>;
    fn cost_date(&self) -> Option<Self::Date>;
    fn cost_label(&self) -> Option<Self::Label>;
    fn cost_merge(&self) -> Option<bool>;

    fn has_price(&self) -> bool;
    fn price_currency(&self) -> Option<Self::Currency>;
    fn price_per_unit(&self) -> Option<Self::Number>;
    fn price_total(&self) -> Option<Self::Number>;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Position<D, N, C, L> {
    pub currency: C,
    pub units: N,
    pub cost: Option<Cost<D, N, C, L>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Cost<D, N, C, L> {
    pub date: D,
    pub per_unit: N,
    pub currency: C,
    pub label: Option<L>,
    pub merge: bool,
}

pub trait Tolerance {
    type Currency;
    type Number;

    fn multipler() -> Self::Number;
    fn for_currency(cur: Self::Currency) -> Option<Self::Number>;
}

pub trait Number: Add + Sub + Sized {
    fn abs(&self) -> Self;
}
