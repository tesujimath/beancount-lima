use std::{
    fmt::Debug,
    ops::{Add, Sub},
};

pub trait Posting<D, A, N, C, L> {
    fn account(&self) -> A;

    fn currency(&self) -> Option<C>;
    fn units(&self) -> Option<N>;

    fn has_cost(&self) -> bool;
    fn cost_currency(&self) -> Option<C>;
    fn cost_per_unit(&self) -> Option<N>;
    fn cost_total(&self) -> Option<N>;
    fn cost_date(&self) -> Option<D>;
    fn cost_label(&self) -> Option<L>;
    fn cost_merge(&self) -> Option<bool>;

    fn has_price(&self) -> bool;
    fn price_currency(&self) -> Option<C>;
    fn price_per_unit(&self) -> Option<N>;
    fn price_total(&self) -> Option<N>;
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

pub trait Inventory<D, A, N, C, L, T> {
    fn account_positions(&self, account: A) -> Vec<Position<D, N, C, L>>;
}

pub trait Tolerance<N, C> {
    fn multipler() -> N;
    fn for_currency(cur: C) -> Option<N>;
}

pub trait Number: Add + Sub + Sized {
    fn abs(&self) -> Self;
}
