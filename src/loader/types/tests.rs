use beancount_parser_lima as parser;
use rust_decimal_macros::dec;
use std::{cmp::Ordering, sync::LazyLock};
use test_case::test_case;
use time::macros::date;

use super::*;

static GBP: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "GBP".try_into().unwrap());
static NZD: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "NZD".try_into().unwrap());
static USD: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "USD".try_into().unwrap());

const fn cost(
    date: Date,
    per_unit: Decimal,
    currency: &'static parser::Currency<'static>,
    label: Option<&'static str>,
    merge: bool,
) -> Cost<'static> {
    Cost {
        per_unit,
        currency,
        date,
        label,
        merge,
    }
}

#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), Some(Ordering::Equal))]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(3.70), &NZD, None, false), None)]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-03), dec!(10.20), &NZD, None, false), Some(Ordering::Less))]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &GBP, None, false), Some(Ordering::Greater))]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, Some("fred"), false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), Some(Ordering::Greater))]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, Some("fred"), false), cost(date!(2020-01-02), dec!(10.20), &NZD, Some("jim"), false), Some(Ordering::Less))]
#[test_case(cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, true), Some(Ordering::Less))]
fn cost_partial_cmp(c0: Cost, c1: Cost, expected: Option<std::cmp::Ordering>) {
    assert_eq!(c0.partial_cmp(&c1), expected);
}
