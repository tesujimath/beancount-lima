#![allow(non_snake_case)]
use super::{booking_test_err, booking_test_ok, Booking, BookingError, PostingBookingError};

// These tests were lifted from:
// https://github.com/beancount/beancount/blob/master/beancount/parser/booking_full_test.py
//
// The meaning of the tags is similar to what was used there, but not identical.
//
// #ante is used for setting up previous positions
// #apply may occur on more than one transaction;  each one is an independent test with these common previous positions
// #ex is used to build an expected inventory, with each of possible multiple apply's being expected to result in the same
// #booked is currently ignored, but could be added
// #reduced is ignored because the Lima booking API does not expose this
// #apply-combined is a new tag;  all transactions tagged as such are combined into a single test

#[test]
fn test_augment__from_empty__no_cost__pos() {
    booking_test_ok(
        r#"
2015-10-01 * #apply
  Assets:Account           1 USD

2015-10-01 * #ex #booked #reduced
  Assets:Account           1 USD
"#,
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
        Booking::Fifo,
    );
}

#[test]
fn test_ambiguous__FIFO__test_matching_more_than_is_available() {
    booking_test_err(
        r#"
2015-01-01 * #ante #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -16 HOOL {}

2015-02-22 * #booked
  error: "Not enough lots to reduce"
"#,
        Booking::Fifo,
        BookingError::Posting(0, PostingBookingError::NotEnoughLotsToReduce),
    );
}

#[test]
fn test_ambiguous__LIFO__no_match_against_any_lots() {
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
        Booking::Lifo,
        // ANOMALY: error
        BookingError::Posting(0, PostingBookingError::CannotInferAnything),
    );
}

#[test]
fn test_ambiguous__LIFO__test_match_against_partial_first_lot() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -2 HOOL {}

2015-02-22 * #booked
  Assets:Account         -2 HOOL {122.22 USD, 2015-10-03}

2015-01-01 * #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          4 HOOL {122.22 USD, 2015-10-03}
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_match_against_complete_first_lot() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -6 HOOL {}

2015-02-22 * #booked
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}

2015-01-01 * #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_partial_match_against_first_two_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account         -7 HOOL {}

2015-02-22 * #booked
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
  Assets:Account         -1 HOOL {111.11 USD, 2015-10-02}

2015-01-01 * #ex
  Assets:Account          4 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_complete_match_against_first_two_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -11 HOOL {}

2015-02-22 * #booked
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}

2015-01-01 * #ex
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_partial_match_against_first_three_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -12 HOOL {}

2015-02-22 * #booked
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account         -1 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
  Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_complete_match_against_first_three_lots() {
    booking_test_ok(
        r#"
2015-01-01 * #ante
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -15 HOOL {}

2015-02-22 * #booked
  Assets:Account         -6 HOOL {122.22 USD, 2015-10-03}
  Assets:Account         -5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account         -4 HOOL {100.00 USD, 2015-10-01}

2015-01-01 * #ex
"#,
        Booking::Lifo,
    );
}

#[test]
fn test_ambiguous__LIFO__test_matching_more_than_is_available() {
    booking_test_err(
        r#"
2015-01-01 * #ante #ex
  Assets:Account          5 HOOL {111.11 USD, 2015-10-02}
  Assets:Account          4 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          6 HOOL {122.22 USD, 2015-10-03}

2015-02-22 * #apply
  Assets:Account        -16 HOOL {}

2015-02-22 * #booked
  error: "Not enough lots to reduce"
"#,
        Booking::Lifo,
        // ANOMALY: error
        BookingError::Posting(0, PostingBookingError::NotEnoughLotsToReduce),
    );
}

// TODO crossing is not supported in OG Beancount:
// test_ambiguous__FIFO__no_match_against_any_lots

// TODO Booking::Average is not supported
// test_ambiguous__AVERAGE__trivial1
// test_ambiguous__AVERAGE__trivial2
// test_ambiguous__AVERAGE__simple_merge2_match1
// test_ambiguous__AVERAGE__simple_merge2_match2
// test_ambiguous__AVERAGE__simple_merge2_match2_b
// test_ambiguous__AVERAGE__simple_merge3_match1
// test_ambiguous__AVERAGE__simple_merge2_insufficient
// test_ambiguous__AVERAGE__simple_merge2_insufficient_b
// test_ambiguous__AVERAGE__mixed_currencies__ambi
// test_ambiguous__AVERAGE__mixed_currencies__unambi_currency
// test_ambiguous__AVERAGE__mixed_currencies__unambi_currency__merging
// test_ambiguous__AVERAGE__mixed_currencies__unambi_cost_ccy__merging
// test_ambiguous__AVERAGE__mixed_currencies__unambi_cost__merging
// test_ambiguous__AVERAGE__mixed_currencies__unambi_date
// test_ambiguous__AVERAGE__mixed_currencies__unambi_with_merge

#[test]
fn test_augment__at_cost__same_date() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {100.00 USD}

2015-10-02 * #apply
  Assets:Account          2 HOOL {100.00 USD, 2015-10-01}

2015-11-01 * #ex
  Assets:Account          3 HOOL {100.00 USD, 2015-10-01}
"#,
        Booking::Strict,
    );
}

#[test]
fn test_augment__at_cost__different_date() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-02 * #apply
  Assets:Account          2 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {100.00 USD, 2015-10-02}

2015-11-01 * #ex
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {100.00 USD, 2015-10-02}
"#,
        Booking::Strict,
    );
}

#[test]
fn test_augment__at_cost__different_cost() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          1 HOOL {100.00 USD}

2015-10-01 * #apply
  Assets:Account          2 HOOL {101.00 USD}

2015-10-01 * #booked
  Assets:Account          2 HOOL {101.00 USD, 2015-10-01}

2015-11-01 * #ex
  Assets:Account          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account          2 HOOL {101.00 USD, 2015-10-01}
"#,
        Booking::Strict,
    );
}

#[test]
fn test_strict_with_size_single() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          1 HOOL {101.00 USD}
  Assets:Account          2 HOOL {102.00 USD}

2015-10-02 * #apply
  Assets:Account         -1 HOOL {}

2015-10-02 * #booked
  Assets:Account         -1 HOOL {101.00 USD, 2015-10-01}

2015-11-04 * #ex
  Assets:Account          2 HOOL {102.00 USD, 2015-10-01}
"#,
        Booking::StrictWithSize,
    );
}

#[test]
fn test_strict_with_size_multiple() {
    booking_test_ok(
        r#"
2015-10-01 * #ante
  Assets:Account          2 HOOL {101.00 USD, 2014-06-02}
  Assets:Account          2 HOOL {102.00 USD, 2014-06-01}

2015-10-02 * #apply
  Assets:Account         -2 HOOL {}

2015-10-02 * #booked
  Assets:Account         -2 HOOL {102.00 USD, 2014-06-01}

2015-11-04 * #ex
  Assets:Account          2 HOOL {101.00 USD, 2014-06-02}
"#,
        Booking::StrictWithSize,
    );
}

#[test]
fn test_combined_augment__at_cost__different_cost() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          1 HOOL {100.00 USD}
  Assets:Other          -100.00 USD

2015-10-01 * "Held-at-cost, positive, different cost" #apply-combined
  Assets:Account1          2 HOOL {101.00 USD}
  Assets:Other          -204.00 USD

2015-10-01 * #booked
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -100.00 USD
  Assets:Account1          2 HOOL {101.00 USD, 2015-10-01}
  Assets:Other          -204.00 USD

2015-10-01 * #ex
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account1          2 HOOL {101.00 USD, 2015-10-01}
  Assets:Other          -304.00 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_augment__at_cost__different_currency() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          1 HOOL {100.00 USD}
  Assets:Other          -100.00 USD

2015-10-01 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1          2 HOOL {100.00 CAD}
  Assets:Other          -200.00 CAD

2015-10-01 * #booked
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -100.00 USD
  Assets:Account1          2 HOOL {100.00 CAD, 2015-10-01}
  Assets:Other          -200.00 CAD

2015-10-01 * #ex
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account1          2 HOOL {100.00 CAD, 2015-10-01}
  Assets:Other          -100.00 USD
  Assets:Other          -200.00 CAD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_augment__at_cost__different_label() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          1 HOOL {100.00 USD}
  Assets:Other          -100.00 USD

2015-10-01 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1          2 HOOL {100.00 USD, "lot1"}
  Assets:Other          -200.00 USD

2015-10-01 * #booked
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -100.00 USD
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01, "lot1"}
  Assets:Other          -200.00 USD

2015-10-01 * #ex
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01, "lot1"}
  Assets:Other          -300.00 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__no_cost() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          10 USD
  Assets:Other1           -10 USD

2015-10-01 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 USD
  Assets:Other2            1 USD

2015-10-01 * #booked
  Assets:Account1          10 USD
  Assets:Other1           -10 USD
  Assets:Account1          -1 USD
  Assets:Other2             1 USD

2015-10-01 * #ex
  Assets:Account1          9 USD
  Assets:Other1          -10 USD
  Assets:Other2            1 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_cost() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {100.00 USD}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__any_spec() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_cost__per() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {100.00}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_cost__total() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -2 HOOL {# 100.00 USD}
  Assets:Other        200.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           200.00 USD

2015-10-01 * #ex
  Assets:Account1          1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -100 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_currency() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {USD}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_date() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {2015-10-01}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}

#[test]
fn test_combined_reduce__same_label() {
    booking_test_ok(
        r#"
2015-10-01 * "Held-at-cost, positive" #apply-combined
  Assets:Account1          3 HOOL {100.00 USD, "6e425dd7b820"}
  Assets:Other       -300.00 USD

2015-10-02 * "Held-at-cost, positive, same cost" #apply-combined
  Assets:Account1         -1 HOOL {"6e425dd7b820"}
  Assets:Other        100.00 USD

2015-10-01 * #booked
  Assets:Account1          3 HOOL {100.00 USD, 2015-10-01, "6e425dd7b820"}
  Assets:Other          -300.00 USD
  Assets:Account1         -1 HOOL {100.00 USD, 2015-10-01, "6e425dd7b820"}
  Assets:Other           100.00 USD

2015-10-01 * #ex
  Assets:Account1          2 HOOL {100.00 USD, 2015-10-01, "6e425dd7b820"}
  Assets:Other          -200 USD
"#,
        Booking::Strict,
    );
}
