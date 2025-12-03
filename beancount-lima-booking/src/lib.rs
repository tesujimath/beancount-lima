mod book;
#[cfg(test)]
pub(crate) use book::book_with_residuals;
pub use book::{book, is_supported_method};

mod categorize;
pub(crate) use categorize::categorize_by_currency;

mod errors;
pub use errors::{BookingError, PostingBookingError, TransactionBookingError};

mod features;

mod interpolate;
pub(crate) use interpolate::{interpolate_from_costed, Interpolation};

mod internal_types;
pub(crate) use internal_types::*;

mod public_types;
pub use public_types::{
    Booking, Bookings, Cost, CostSpec, Interpolated, Inventory, Number, Position, Positions,
    Posting, PostingCost, PostingCosts, PostingSpec, Price, PriceSpec, Sign, Tolerance,
};

mod reductions;
pub(crate) use reductions::{book_reductions, Reductions};

#[cfg(test)]
mod tests;
