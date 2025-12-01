use super::{Booking, BookingError, PostingBookingError, TransactionBookingError};

// a subset of the tests from
// https://github.com/beancount/beancount/blob/master/beancount/parser/booking_full_test.py

#[test]
fn test_augment__from_empty__no_cost__pos() {
    booking_test_ok(
        r#"
2015-10-01 * #apply
  Assets:Account           1 USD

2015-10-01 * #ex #booked #reduced
  Assets:Account           1 USD
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_augment__from_empty__no_cost__neg() {
    booking_test_ok(
        r#"
2015-10-01 * #apply
  Assets:Account          -1 USD

2015-10-01 * #ex #booked #reduced
  Assets:Account           -1 USD
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_augment__from_empty__at_cost__pos() {
    booking_test_ok(
        r#"
2015-10-01 * #apply
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #ex #booked
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}

2015-10-01 * #reduced
  'S Assets:Account       1 HOOL {100.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_augment__from_empty__at_cost__neg() {
    booking_test_ok(
        r#"
2015-10-01 * #apply
  Assets:Account          -1 HOOL {100.00 USD}

2015-10-01 * #ex #booked
  Assets:Account          -1 HOOL {100.00 USD, 2015-10-01}

2015-10-01 * #reduced
  'S Assets:Account       -1 HOOL {100.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_augment__from_empty__incomplete_cost__empty() {
    booking_test_err(
        r#"
2015-10-01 * #apply
  Assets:Account          1 HOOL {}

2015-10-01 * #booked
  error: "Failed to categorize posting"
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::CannotInferAnything),
    );
}

#[test]
fn test_augment__from_empty__incomplete_cost__with_currency() {
    booking_test_err(
        r#"
2015-10-01 * #apply
  Assets:Account          1 HOOL {USD}

2015-10-01 * #booked
  Assets:Account          1 HOOL {0 USD, 2015-10-01}

2015-10-01 * #reduced
  'S Assets:Account       1 HOOL {USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
        // ANOMALY: original test was different, but this seems correct to me
        BookingError::Posting(0, PostingBookingError::CannotInferUnits),
    );
}

#[test]
fn test_reduce__no_cost() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          10 USD

2015-10-01 * #apply #booked #reduced
  Assets:Account          -5 USD

2015-10-01 * #ex
  Assets:Account           5 USD
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_reduce__sign_change_simple() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account         10 HOOL {33.33 USD, 2016-01-01}

2016-05-08 * #apply
  Assets:Account        -13 HOOL {}

2016-05-08 * #booked
  error: "Not enough lots to reduce"

2016-01-01 * #ex
  Assets:Account         10 HOOL {33.33 USD, 2016-01-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::NotEnoughLotsToReduce),
    );
}

#[test]
fn test_reduce__no_match() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account          10 HOOL {123.45 USD, 2016-04-15}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {123.00 USD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {123.45 CAD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {123.45 USD, 2016-04-16}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {123.45 USD, "lot1"}

2016-05-02 * #booked
  error: "No position matches"
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::NoPositionMatches),
    );
}

#[test]
fn test_reduce__unambiguous() {
    booking_test_ok(
        r#"
2016-01-01 * #ante #ambi-matches
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {}

2016-05-02 * #booked #ambi-resolved #reduced
  Assets:Account          -5 HOOL {115.00 USD, 2016-04-15, "lot1"}

2016-01-01 * #ex
  Assets:Account           5 HOOL {115.00 USD, 2016-04-15, "lot1"}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_reduce__ambiguous__strict() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot2"}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {115.00 USD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {USD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {2016-04-15}

2016-05-02 * #booked
  error: "Ambiguous matches"

2016-05-02 * #ex
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot1"}
  Assets:Account          10 HOOL {115.00 USD, 2016-04-15, "lot2"}
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::AmbiguousMatches),
    );
}

mod helpers;
use helpers::*;
