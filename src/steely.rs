// TODO remove:
#![allow(dead_code, unused_variables)]
use parking_lot::{RawRwLock, RwLockReadGuard};
use std::{
    any::{Any, TypeId},
    fmt::Display,
    marker::PhantomData,
    ops::{Add, AddAssign},
};
use steel::{
    gc::{Gc, GcMut},
    rvals::{CustomType, IntoSteelVal, MaybeSendSyncStatic},
    SteelVal,
};

// A wrapper for any type implementing `MaybeSendSyncStatic` to make it efficiently sharable
// between Rust and Scheme, i.e. without allocation.
#[derive(Clone)]
pub(crate) struct Steely<T>
where
    T: Clone + MaybeSendSyncStatic,
{
    value: GcMut<Box<dyn CustomType>>,
    _type: PhantomData<T>,
}

impl<T> Steely<T>
where
    T: Clone + MaybeSendSyncStatic,
{
    pub(crate) fn f1<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let value = self.value.read();
        let value = value.as_any_ref().downcast_ref::<T>().unwrap();

        f(value)
    }

    pub(crate) fn f2<F, R>(&self, other: &Self, f: F) -> R
    where
        F: FnOnce(&T, &T) -> R,
    {
        let value = self.value.read();
        let value = value.as_any_ref().downcast_ref::<T>().unwrap();
        let other = other.value.read();
        let other = other.as_any_ref().downcast_ref::<T>().unwrap();

        f(value, other)
    }

    pub(crate) fn map<F, R>(&mut self, other: &Self, f: F) -> Steely<R>
    where
        F: FnOnce(&T) -> R,
        R: CustomType + Clone + MaybeSendSyncStatic,
    {
        let value = self.value.read();
        let value = value.as_any_ref().downcast_ref::<T>().unwrap();

        f(value).into()
    }

    pub(crate) fn map2<F, R>(&self, other: &Self, f: F) -> Steely<R>
    where
        F: FnOnce(&T, &T) -> R,
        R: CustomType + Clone + MaybeSendSyncStatic,
    {
        let value = self.value.read();
        let value = value.as_any_ref().downcast_ref::<T>().unwrap();
        let other = other.value.read();
        let other = other.as_any_ref().downcast_ref::<T>().unwrap();

        f(value, other).into()
    }
}

impl<T> IntoSteelVal for Steely<T>
where
    T: Clone + MaybeSendSyncStatic,
{
    fn into_steelval(self) -> steel::rvals::Result<SteelVal> {
        Ok(SteelVal::Custom(self.value.clone()))
    }
}

impl<T> From<T> for Steely<T>
where
    T: CustomType + Clone + MaybeSendSyncStatic,
{
    fn from(value: T) -> Self {
        Self {
            value: Gc::new_mut(Box::new(value)),
            _type: PhantomData::<T>,
        }
    }
}

impl<T> PartialEq for Steely<T>
where
    T: Clone + MaybeSendSyncStatic + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.f2(other, |value, other| value.eq(other))
    }
}

impl<T> PartialOrd for Steely<T>
where
    T: Clone + MaybeSendSyncStatic + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.f2(other, |value, other| value.partial_cmp(other))
    }
}

impl<T> Eq for Steely<T> where T: Clone + MaybeSendSyncStatic + Eq {}

impl<T> Ord for Steely<T>
where
    T: Clone + MaybeSendSyncStatic + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.f2(other, |value, other| value.cmp(other))
    }
}

impl<T> Display for Steely<T>
where
    T: Clone + MaybeSendSyncStatic + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.f1(|value| value.fmt(f))
    }
}

impl<T> std::fmt::Debug for Steely<T>
where
    T: Clone + MaybeSendSyncStatic + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.f1(|value| value.fmt(f))
    }
}
