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

impl Display for BookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BookingError::*;

        match self {
            Transaction(e) => write!(f, "{e}"),
            Posting(idx, e) => write!(f, "posting {idx} {e}"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TransactionBookingError {
    UnsupportedBookingMethod(Booking, String),
    TooManyMissingNumbers,
    Unbalanced(String),
    CannotDetermineCurrencyForBalancing,
    AutoPostMultipleBuckets(Vec<String>),
}

impl Display for TransactionBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TransactionBookingError::*;

        match self {
            UnsupportedBookingMethod(booking, account) => {
                write!(f, "unsupported booking method {booking} for {account}")
            }
            TooManyMissingNumbers => f.write_str("too many missing numbers for interpolation"),
            Unbalanced(residual) => write!(f, "unbalanced transaction with residual {residual}"),
            CannotDetermineCurrencyForBalancing => {
                f.write_str("can't determine currency for balancing transaction")
            }
            AutoPostMultipleBuckets(buckets) => write!(
                f,
                "can't have auto-post with multiple currencies {}",
                buckets.join(","),
            ),
        }
    }
}

impl Error for TransactionBookingError {}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PostingBookingError {
    AmbiguousAutoPost,
    AmbiguousMatches,
    MultipleCostCurrenciesMatch,
    CannotInferUnits,
    CannotInferCurrency,
    CannotInferAnything,
    CannotInferPricePerUnit,
    CannotInferPriceCurrency,
    CannotInferPrice,
    NotEnoughLotsToReduce,
    NoPositionMatches,
}

impl Display for PostingBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PostingBookingError::*;

        match self {
            AmbiguousAutoPost => f.write_str("ambiguous auto-post"),
            AmbiguousMatches => f.write_str("ambiguous matches"),
            MultipleCostCurrenciesMatch => {
                f.write_str("multiple currencies in cost spec matches against inventory")
            }
            CannotInferUnits => f.write_str("cannot infer units"),
            CannotInferCurrency => f.write_str("cannot infer currency"),
            CannotInferAnything => f.write_str("cannot infer anything"),
            CannotInferPricePerUnit => f.write_str("cannot infer price per-unit"),
            CannotInferPriceCurrency => f.write_str("cannot infer price currency"),
            CannotInferPrice => f.write_str("cannot infer price"),
            NotEnoughLotsToReduce => f.write_str("not enough lots to reduce"),
            NoPositionMatches => f.write_str("no position matches"),
        }
    }
}
