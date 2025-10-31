use std::{
    error::Error,
    fmt::{Debug, Display},
};

use super::Booking;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BookingError {
    Transaction(TransactionBookingError),
    Posting(usize, PostingBookingError),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TransactionBookingError {
    UnsupportedBookingMethod(Booking, String),
    TooManyMissingNumbers,
    NoResidualForInterpolation,
    Unbalanced(String),
}

impl Display for TransactionBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TransactionBookingError::*;

        match self {
            UnsupportedBookingMethod(booking, account) => {
                write!(f, "unsupported booking method {booking} for {account}")
            }
            TooManyMissingNumbers => f.write_str("too many missing numbers for interpolation"),
            NoResidualForInterpolation => f.write_str("no residual for interpolation"),
            Unbalanced(residual) => write!(f, "unbalanced transaction with residual {residual}"),
        }
    }
}

impl Error for TransactionBookingError {}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PostingBookingError {
    CannotCategorize,
    AmbiguousAutoPost,
    MultipleCostSpecMatches,
    MultipleCostCurrenciesMatch,
    CannotInferUnits,
    CannotInferCurrency,
    CannotInferAnything,
}

impl Display for PostingBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PostingBookingError::*;

        match self {
            CannotCategorize => f.write_str("cannot categorize posting"),
            AmbiguousAutoPost => f.write_str("ambiguous auto-post"),
            MultipleCostSpecMatches => f.write_str("multiple cost spec matches against inventory"),
            MultipleCostCurrenciesMatch => {
                f.write_str("multiple currencies in cost spec matches against inventory")
            }
            CannotInferUnits => f.write_str("cannot infer units"),
            CannotInferCurrency => f.write_str("cannot infer currency"),
            CannotInferAnything => f.write_str("cannot infer anything"),
        }
    }
}
