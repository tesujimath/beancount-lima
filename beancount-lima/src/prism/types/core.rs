// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use std::ops::Deref;
use steel::{gc::Shared, rvals::Custom};

/// For third-party types we need to share
#[derive(Clone, Debug)]
pub(crate) struct CustomShared<T>(Shared<T>);

impl<T> From<T> for CustomShared<T> {
    fn from(value: T) -> Self {
        CustomShared(Shared::new(value))
    }
}

impl<T> Deref for CustomShared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl<T> Custom for CustomShared<T> where T: 'static {}
