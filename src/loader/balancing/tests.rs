use super::*;
use beancount_parser_lima::{self as parser, BeancountParser, BeancountSources, ParseSuccess};
use rust_decimal_macros::dec;
use speculoos::prelude::*;
use test_case::test_case;

use crate::tests::init_tracing;

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) struct StaticWeight {
    pub(crate) number: rust_decimal::Decimal,
    pub(crate) currency: String,
    pub(crate) source: StaticWeightSource,
}

impl<'a> From<Weight<'a>> for StaticWeight {
    fn from(value: Weight) -> Self {
        StaticWeight {
            number: value.number,
            currency: value.currency.to_string(),
            source: value.source.into(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum StaticWeightSource {
    Native,
    Cost(rust_decimal::Decimal, String),
    Price(rust_decimal::Decimal, String),
}

impl<'a> From<WeightSource<'a>> for StaticWeightSource {
    fn from(value: WeightSource<'a>) -> Self {
        use StaticWeightSource::*;
        match value {
            WeightSource::Native => Native,
            WeightSource::Cost(num, cur) => Cost(num, cur.to_string()),
            WeightSource::Price(num, cur) => Price(num, cur.to_string()),
        }
    }
}

impl From<((rust_decimal::Decimal, &'static str), StaticWeightSource)> for StaticWeight {
    fn from(value: ((rust_decimal::Decimal, &'static str), StaticWeightSource)) -> Self {
        StaticWeight {
            number: value.0 .0,
            currency: value.0 .1.to_string(),
            source: value.1,
        }
    }
}

fn parse_txn_and_balance(src: &str) -> Result<Vec<StaticWeight>, (String, String)> {
    let sources = BeancountSources::from(src);
    let parser = BeancountParser::new(&sources);
    let ParseSuccess {
        directives,
        options,
        ..
    } = parser.parse().unwrap();
    assert_eq!(directives.len(), 1);
    let d = directives.first().unwrap();
    if let parser::DirectiveVariant::Transaction(txn) = d.variant() {
        let inferred_tolerance = InferredTolerance::new(&options);
        let element = into_spanned_element(d);

        balance_transaction(txn, &inferred_tolerance, &element)
            .map(|weights| {
                weights
                    .into_iter()
                    .map(Into::<StaticWeight>::into)
                    .collect::<Vec<_>>()
            })
            .map_err(|e| {
                let e = e.error_or_warning;
                (e.to_string(), sources.error_source_text(&e).to_string())
            })
    } else {
        panic!("unexpected non-transaction");
    }
}

#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Expenses:E1 -100.00 USD
",
    vec![
        (dec!(100.00), "USD"),
        (dec!(-100.00), "USD"),
    ]
)]
fn native_no_inference(src: &'static str, expected: Vec<(rust_decimal::Decimal, &'static str)>) {
    init_tracing();
    let result = parse_txn_and_balance(src);
    let expected = expected
        .into_iter()
        .map(|expected| Into::<StaticWeight>::into((expected, StaticWeightSource::Native)))
        .collect::<Vec<_>>();
    assert_eq!(&result.unwrap(), &expected);
}

#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Expenses:E1
",
    vec![
        (dec!(100.00), "USD"),
        (dec!(-100.00), "USD"),
    ]
)]
#[test_case(
    "
2025-01-01 txn
  Assets:A1   -100.00 USD
  Expenses:E1   75.00
  Expenses:E2
",
    vec![
        (dec!(-100.00), "USD"),
        (dec!(75.00), "USD"),
        (dec!(25.00), "USD"),
    ]
)]
#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Assets:A2    50.00 NZD
  Expenses:E1        USD
  Expenses:E2        NZD
",
    vec![
        (dec!(100.00), "USD"),
        (dec!(50.00), "NZD"),
        (dec!(-100.00), "USD"),
        (dec!(-50.00), "NZD"),
    ]
)]
#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Assets:A2    50.00 NZD
  Expenses:E1        USD
  Expenses:E2
",
    vec![
        (dec!(100.00), "USD"),
        (dec!(50.00), "NZD"),
        (dec!(-100.00), "USD"),
        (dec!(-50.00), "NZD"),
    ]
)]
fn native_inference(src: &'static str, expected: Vec<(rust_decimal::Decimal, &'static str)>) {
    init_tracing();
    let result = parse_txn_and_balance(src);
    let expected = expected
        .into_iter()
        .map(|expected| Into::<StaticWeight>::into((expected, StaticWeightSource::Native)))
        .collect::<Vec<_>>();
    assert_eq!(&result.unwrap(), &expected);
}

#[test_case(
    "
2025-01-01 txn
  Assets:A1    100.00 USD
  Assets:A2     50.00 NZD
  Expenses:E1  -99.00 USD
  Expenses:E2  -51.00 NZD
",
    "invalid transaction (balancing error -1.00 NZD, 1.00 USD)"
)]
fn native_balancing_errors(src: &str, expected_error: &str) {
    init_tracing();
    let result = parse_txn_and_balance(src);
    if let Err((error, _source)) = result {
        assert_that(&error).contains(expected_error);
    } else {
        panic!("Unexpected success: {result:?}")
    }
}

#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Assets:A2    50.00 NZD
  Expenses:E1 100.00
",
    "can't infer currency",
    "Expenses:E1 100.00"
)]
#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 USD
  Assets:A2    50.00 NZD
  Expenses:E1
",
    "can't infer anything",
    "Expenses:E1"
)]
fn native_inference_errors(src: &str, expected_error: &str, expected_source: &str) {
    init_tracing();
    let result = parse_txn_and_balance(src);
    if let Err((error, source)) = result {
        assert_that(&error).contains(expected_error);
        assert_that(&source.as_str()).is_equal_to(expected_source);
    } else {
        panic!("Unexpected success: {result:?}")
    }
}

#[test_case(
    "
2025-01-01 txn
  Assets:A1   100.00 NZD
  Assets:A2   -50.00 GBP @
",
    vec![
        ((dec!(100.00), "NZD"), StaticWeightSource::Native),
        ((dec!(-100.00), "NZD"), StaticWeightSource::Price(dec!(-50.00), "GBP".to_string()) ),
    ]
)]
fn price_inference(
    src: &'static str,
    expected: Vec<((rust_decimal::Decimal, &'static str), StaticWeightSource)>,
) {
    init_tracing();
    let result = parse_txn_and_balance(src);
    let expected = expected
        .into_iter()
        .map(Into::<StaticWeight>::into)
        .collect::<Vec<_>>();
    assert_eq!(&result.unwrap(), &expected);
}
