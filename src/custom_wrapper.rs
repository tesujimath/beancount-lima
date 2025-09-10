use std::fmt::Display;

use steel::rvals::{as_underlying_type, Custom, CustomType};

#[derive(Clone)]
pub(crate) struct CustomWrapperWithEq<T>(T)
where
    T: Clone;

impl<T> Custom for CustomWrapperWithEq<T>
where
    T: Clone + Display + PartialEq + 'static,
{
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<CustomWrapperWithEq<T>>(other) {
            self.0 == other.0
        } else {
            false
        }
    }
}
