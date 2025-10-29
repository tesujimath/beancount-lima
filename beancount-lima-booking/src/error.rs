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
}

impl Display for TransactionBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TransactionBookingError::*;

        match self {
            UnsupportedBookingMethod(booking, account) => {
                write!(f, "unsupported booking method {booking} for {account}")
            }
        }
    }
}

impl Error for TransactionBookingError {}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PostingBookingError {
    FailedToCategorize,
    AmbiguousAutoPost,
    MultipleCostSpecMatches,
    MultipleCostCurrenciesMatch,
}

impl Display for PostingBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PostingBookingError::*;

        match self {
            FailedToCategorize => f.write_str("failed to categorize posting"),
            AmbiguousAutoPost => f.write_str("ambiguous auto-post"),
            MultipleCostSpecMatches => f.write_str("multiple cost spec matches against inventory"),
            MultipleCostCurrenciesMatch => {
                f.write_str("multiple currencies in cost spec matches against inventory")
            }
        }
    }
}
