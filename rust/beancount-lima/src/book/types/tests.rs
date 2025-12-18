use beancount_parser_lima as parser;
use rstest::rstest;
use rust_decimal_macros::dec;
use std::{cmp::Ordering, sync::LazyLock};
use time::macros::date;

use super::*;

static GBP: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "GBP".try_into().unwrap());
static NZD: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "NZD".try_into().unwrap());
static USD: LazyLock<parser::Currency<'static>> = LazyLock::new(|| "USD".try_into().unwrap());

const fn cost(
    date: Date,
    per_unit: Decimal,
    currency: &parser::Currency<'static>,
    label: Option<&'static str>,
    merge: bool,
) -> Cost<'static> {
    Cost {
        per_unit,
        currency: *currency,
        date,
        label,
        merge,
    }
}

#[rstest]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, false)), Ordering::Equal)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(3.70), &NZD, None, false)), Ordering::Greater)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-03), dec!(10.20), &NZD, None, false)), Ordering::Less)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &GBP, None, false)), Ordering::Greater)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, Some("fred"), false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, false)), Ordering::Greater)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, Some("fred"), false), cost(date!(2020-01-02), dec!(10.20), &NZD, Some("jim"), false)), Ordering::Less)]
#[case((cost(date!(2020-01-02), dec!(10.20), &NZD, None, false), cost(date!(2020-01-02), dec!(10.20), &NZD, None, true)), Ordering::Less)]
fn cost_cmp(#[case] input: (Cost, Cost), #[case] expected: std::cmp::Ordering) {
    let (c0, c1) = input;
    assert_eq!(c0.cmp(&c1), expected);
}
