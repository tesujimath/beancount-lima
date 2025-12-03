#![allow(non_snake_case)]
use super::{Booking, BookingError, PostingBookingError};

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

#[test]
fn test_reduce__ambiguous__none() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account           1 HOOL {115.00 USD}
  Assets:Account           2 HOOL {116.00 USD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {117.00 USD}

2016-05-02 * #booked
  Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}

2016-05-02 * #reduced
  'S Assets:Account        -5 HOOL {117.00 USD, 2016-05-02}

2016-01-01 * #ex
  Assets:Account           1 HOOL {115.00 USD, 2016-01-01}
  Assets:Account           2 HOOL {116.00 USD, 2016-01-01}
  Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}
"#,
        NO_OPTIONS,
        Booking::None,
    );
}

#[test]
fn test_reduce__ambiguous__none__from_mixed() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account           1 HOOL {115.00 USD}
  Assets:Account          -2 HOOL {116.00 USD}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {117.00 USD}

2016-05-02 * #booked
  Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}

2016-05-02 * #reduced
  'S Assets:Account        -5 HOOL {117.00 USD, 2016-05-02}

2016-01-01 * #ex
  Assets:Account           1 HOOL {115.00 USD, 2016-01-01}
  Assets:Account          -2 HOOL {116.00 USD, 2016-01-01}
  Assets:Account          -5 HOOL {117.00 USD, 2016-05-02}
"#,
        NO_OPTIONS,
        Booking::None,
    );
}

#[test]
fn test_reduce__other_currency() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account           8 AAPL {115.00 USD, 2016-01-11}
  Assets:Account           8 HOOL {115.00 USD, 2016-01-10}

2016-01-01 * #ambi-matches
  Assets:Account           8 HOOL {115.00 USD, 2016-01-10}

2016-01-01 * #ambi-resolved
  Assets:Account          -5 HOOL {115.00 USD, 2016-01-10}

2016-05-02 * #apply
  Assets:Account          -5 HOOL {115.00 USD}

2016-05-02 * #booked #reduced
  Assets:Account          -5 HOOL {115.00 USD, 2016-01-10}

2016-01-01 * #ex
  Assets:Account           8 AAPL {115.00 USD, 2016-01-11}
  Assets:Account           3 HOOL {115.00 USD, 2016-01-10}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_reduce__multiple_reductions() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           50 HOOL {116.00 USD, 2016-01-16}

2016-05-02 * #apply
  Assets:Account          -40 HOOL {}
  Assets:Account          -35 HOOL {}

2016-05-02 * #booked
  Assets:Account          -40 HOOL {115.00 USD, 2016-01-15}
  Assets:Account          -10 HOOL {115.00 USD, 2016-01-15}
  Assets:Account          -25 HOOL {116.00 USD, 2016-01-16}

2016-01-01 * #ex
  Assets:Account           25 HOOL {116.00 USD, 2016-01-16}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_reduce__multiple_reductions_hifo() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           50 HOOL {116.00 USD, 2016-01-16}
  Assets:Account           50 HOOL {114.00 USD, 2016-01-17}

2016-05-02 * #apply
  Assets:Account          -40 HOOL {}
  Assets:Account          -35 HOOL {}
  Assets:Account          -30 HOOL {}

2016-05-02 * #booked
  Assets:Account          -40 HOOL {116.00 USD, 2016-01-16}
  Assets:Account          -10 HOOL {116.00 USD, 2016-01-16}
  Assets:Account          -25 HOOL {115.00 USD, 2016-01-15}
  Assets:Account          -25 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           -5 HOOL {114.00 USD, 2016-01-17}

2016-01-01 * #ex
  Assets:Account           45 HOOL {114.00 USD, 2016-01-17}
"#,
        NO_OPTIONS,
        Booking::Hifo,
    );
}

#[test]
fn test_reduce__multiple_reductions__competing__with_error() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account            5 HOOL {115.00 USD, 2016-01-15}

2016-05-02 * #apply
  Assets:Account           -4 HOOL {115.00 USD}
  Assets:Account           -4 HOOL {2016-01-15}

2016-05-02 * #booked
  error: "Not enough lots to reduce"
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(1, PostingBookingError::NotEnoughLotsToReduce),
    );
}

#[test]
fn test_reduce__multiple_reductions__overflowing__with_error() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account           50 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           50 HOOL {116.00 USD, 2016-01-16}

2016-05-02 * #apply
  Assets:Account          -40 HOOL {}
  Assets:Account          -65 HOOL {}

2016-05-02 * #booked
  error: "Not enough lots to reduce"
"#,
        NO_OPTIONS,
        Booking::Fifo,
        BookingError::Posting(1, PostingBookingError::NotEnoughLotsToReduce),
    );
}

#[test]
fn test_reduce__multiple_reductions__no_error_because_total() {
    booking_test_ok(
        r#"
2016-01-01 * #ante
  Assets:Account            7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account            4 HOOL {115.00 USD, 2016-01-16}
  Assets:Account            3 HOOL {117.00 USD, 2016-01-15}

2016-05-02 * #apply
  Assets:Account          -11 HOOL {115.00 USD}

2016-01-01 * #ambi-matches
  Assets:Account            7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account            4 HOOL {115.00 USD, 2016-01-16}

2016-01-01 * #ambi-resolved #booked
  Assets:Account           -7 HOOL {115.00 USD, 2016-01-15}
  Assets:Account           -4 HOOL {115.00 USD, 2016-01-16}

; ANOMALY: added ex
2016-01-01 * #ex
  Assets:Account            3 HOOL {117.00 USD, 2016-01-15}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

#[test]
fn test_reduce__reduction_with_same_currency_not_at_cost() {
    booking_test_err(
        r#"
2016-01-01 * #ante
  Assets:Account   50 HOOL @ 14.33 USD

2016-05-02 * #apply
  Assets:Account  -40 HOOL {14.33 USD} @ 14.33 USD

2016-05-02 * #booked
  error: "No position matches"
"#,
        NO_OPTIONS,
        Booking::Fifo,
        BookingError::Posting(0, PostingBookingError::NoPositionMatches),
    );
}

#[test]
fn test_reduce__missing_units_number() {
    booking_test_ok(
        r#"
2016-01-01 * #ante

2016-05-02 * #apply
  Assets:Account              HOOL {115.00 USD}

2016-01-01 * #booked

; ANOMALY: added ex, units inferred as zero ???
2016-01-01 * #ex
  Assets:Account            0 HOOL {115.00 USD, 2016-05-02}
"#,
        NO_OPTIONS,
        Booking::Strict,
    );
}

// TODO self reductions tests:
// test_has_self_reductions__simple
// test_has_self_reductions__inverted_signs
// test_has_self_reductions__multiple
// test_has_self_reductions__reducing_without_cost
// test_has_self_reductions__augmenting_without_cost
// test_has_self_reductions__different_currency
// test_has_self_reductions__different_account
// test_has_self_reductions__total_replacement
// test_has_self_reductions__booking_method_allowed

// TODO more self reductions tests, also not handled by OG Beancount:
// test_reduce__augment_and_reduce_with_empty_balance
// test_reduce__augment_and_reduce_with_empty_balance__matching_pos
// test_reduce__augment_and_reduce_with_empty_balance__matching_neg
// test_reduce__augment_and_reduce_with_non_empty_balance

#[test]
fn test_ambiguous__NONE__matching_existing1() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__NONE__matching_existing2() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {101.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          3 HOOL {101.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__NONE__notmatching_nonmixed1() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
"#,
        NO_OPTIONS,
        Booking::None,
    );
}

#[test]
fn test_ambiguous__NONE__notmatching_mixed1() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}
"#,
        NO_OPTIONS,
        Booking::None,
    );
}

#[test]
fn test_ambiguous__NONE__notmatching_mixed2() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply #booked
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {102.00 USD, 2015-06-01}
"#,
        NO_OPTIONS,
        Booking::None,
    );
}

#[test]
fn test_ambiguous__STRICT_1() {
    booking_test_err(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {102.00 USD}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {2015-06-01}

2015-06-01 * #booked
  error: "No position matches"

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::NoPositionMatches),
    );
}

#[test]
fn test_ambiguous__STRICT_2() {
    booking_test_err(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -6 HOOL {100.00 USD, 2015-10-01}

2015-06-01 * #booked
  error: "Not enough lots to reduce"

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          5 HOOL {101.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::NotEnoughLotsToReduce),
    );
}

#[test]
fn test_ambiguous__STRICT__mixed() {
    booking_test_err(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {102.00 USD, 2015-06-01}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {102.00 USD}

2015-06-01 * #apply
  Assets:Account         -2 HOOL {2015-06-01}

2015-06-01 * #booked
  error: "No position matches"

2015-01-01 * #ex
  Assets:Account          5 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {101.00 USD, 2015-10-01}
"#,
        NO_OPTIONS,
        Booking::Strict,
        BookingError::Posting(0, PostingBookingError::NoPositionMatches),
    );
}

#[test]
fn test_ambiguous__FIFO__no_match_against_any_lots() {
    booking_test_err(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account          0 HOOL {}

2015-02-22 * #reduced
  'S Assets:Account          0 HOOL {USD, 2015-02-22}

2015-02-22 * #booked

2015-01-01 * #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
        // ANOMALY: error
        BookingError::Posting(0, PostingBookingError::CannotInferAnything),
    );
}

#[test]
fn test_ambiguous__FIFO__test_match_against_partial_first_lot() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -2 HOOL {}

2015-02-22 * #booked
  Assets:Account         -2 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_match_against_complete_first_lot() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -4 HOOL {}

2015-02-22 * #booked
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_partial_match_against_first_two_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -7 HOOL {}

2015-02-22 * #booked
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -3 HOOL {111.11 USD, 2015-10-02}

2015-01-01 * #ex
  Assets:Account          2 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_complete_match_against_first_two_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -9 HOOL {}

2015-02-22 * #booked
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}

2015-01-01 * #ex
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_partial_match_against_first_three_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -12 HOOL {}

2015-02-22 * #booked
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account         -3 HOOL {122.22 USD, 2015-10-03}

2015-01-01 * #ex
  Assets:Account          3 HOOL {122.22 USD, 2015-10-03}
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_complete_match_against_first_three_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -15 HOOL {}

2015-02-22 * #booked
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}

2015-01-01 * #ex
"#,
        NO_OPTIONS,
        Booking::Fifo,
    );
}

mod helpers;
use helpers::*;
