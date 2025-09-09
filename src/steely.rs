// TODO remove:
#![allow(dead_code, unused_variables)]
// TODO why is this here?
use color_eyre::eyre::Result;
use std::{
    fmt::Display,
    ops::{AddAssign, Deref, DerefMut},
};
use steel::rvals::{as_underlying_type, Custom, CustomType};

// a wrapper for any type implementing clone to make it compatiable with Steel
// PartialEq requirement is a bit crass, and is here for now to support SteelDecimal
#[derive(Clone, Debug)]
pub(crate) struct Steely<T>(T)
where
    T: Clone;

impl<T> Custom for Steely<T>
where
    T: Clone + Display + PartialEq + 'static,
{
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Steely<T>>(other) {
            self == other
        } else {
            false
        }
    }
}

impl<T> Copy for Steely<T> where T: Clone + Copy {}

impl<T> From<T> for Steely<T>
where
    T: Clone,
{
    fn from(value: T) -> Self {
        Steely(value)
    }
}

impl<T> PartialEq for Steely<T>
where
    T: Clone + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> PartialOrd for Steely<T>
where
    T: Clone + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Eq for Steely<T> where T: Clone + Eq {}

impl<T> Ord for Steely<T>
where
    T: Clone + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Display for Steely<T>
where
    T: Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Deref for Steely<T>
where
    T: Clone,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Steely<T>
where
    T: Clone,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> AsRef<T> for Steely<T>
where
    T: Clone,
{
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Steely<T>
where
    T: Clone,
{
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> AddAssign<Steely<T>> for Steely<T>
where
    T: Clone + AddAssign,
{
    fn add_assign(&mut self, rhs: Steely<T>) {
        self.as_mut().add_assign(rhs.as_ref().clone());
    }
}
