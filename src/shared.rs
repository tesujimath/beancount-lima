// TODO remove:
#![allow(dead_code, unused_variables)]
use std::{
    fmt::Display,
    ops::Deref,
    sync::{Arc, RwLock},
};
use steel::rvals::Custom;

// a wrapper for efficiently sharing a Rust type with Scheme
#[derive(Clone, Debug)]
pub(crate) struct Shared<T>(Arc<RwLock<T>>)
where
    T: Clone;

impl<T> Custom for Shared<T> where T: Clone + 'static {}

impl<T> From<T> for Shared<T>
where
    T: Clone,
{
    fn from(value: T) -> Self {
        Shared(Arc::new(RwLock::new(value)))
    }
}

impl<T> PartialEq for Shared<T>
where
    T: Clone + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let this = self.0.read().unwrap();
        let other = other.0.read().unwrap();
        this.eq(&other)
    }
}

impl<T> PartialOrd for Shared<T>
where
    T: Clone + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let this = self.0.read().unwrap();
        let other = other.0.read().unwrap();
        this.partial_cmp(&other)
    }
}

impl<T> Eq for Shared<T> where T: Clone + Eq {}

impl<T> Ord for Shared<T>
where
    T: Clone + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let this = self.0.read().unwrap();
        let other = other.0.read().unwrap();
        this.cmp(&other)
    }
}

impl<T> Display for Shared<T>
where
    T: Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let this = self.0.read().unwrap();
        this.fmt(f)
    }
}

impl<T> Deref for Shared<T>
where
    T: Clone,
{
    type Target = RwLock<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
