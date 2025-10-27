use std::{
    error::Error,
    fmt::{Debug, Display},
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BookingError {
    Transaction(TransactionBookingError),
    Posting(usize, PostingBookingError),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TransactionBookingError {
    Unbalanced, // TODO
}

impl Display for TransactionBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TransactionBookingError::*;

        match self {
            Unbalanced => f.write_str("unbalanced transaction"),
        }
    }
}

impl Error for TransactionBookingError {}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PostingBookingError {
    FailedToCategorize,
}

impl Display for PostingBookingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PostingBookingError::*;

        match self {
            FailedToCategorize => f.write_str("failed to categorize posting"),
        }
    }
}
