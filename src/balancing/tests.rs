use super::*;
use beancount_parser_lima::{self as parser, BeancountParser, BeancountSources, ParseSuccess};
use rust_decimal_macros::dec;
use speculoos::prelude::*;
use test_case::test_case;

fn parse_txn_and_balance(
    src: &str,
) -> Result<Vec<(rust_decimal::Decimal, String, WeightSource)>, (String, String)> {
    let sources = BeancountSources::from(src);
    let parser = BeancountParser::new(&sources);
    let ParseSuccess { directives, .. } = parser.parse().unwrap();
    assert_eq!(directives.len(), 1);
    let d = directives.first().unwrap();
    if let parser::DirectiveVariant::Transaction(txn) = d.variant() {
        let element = Into::<WrappedSpannedElement>::into(d);
        let weights = determine_transaction_weight(txn, element)
            .map(|weights| {
                weights
                    .into_iter()
                    .map(|w| (w.number, w.currency.to_string(), w.source))
                    .collect::<Vec<_>>()
            })
            .map_err(|e| {
                let e = e.error_or_warning;
                (e.to_string(), sources.error_source_text(&e).to_string())
            });
        weights
    } else {
        panic!("unexpected non-transaction");
    }
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
fn native_inference(src: &str, expected: Vec<(rust_decimal::Decimal, &str)>) {
    let result = parse_txn_and_balance(src);
    let expected = expected
        .into_iter()
        .map(|(num, cur)| (num, cur.to_string(), WeightSource::Native))
        .collect::<Vec<_>>();
    assert_eq!(&result.unwrap(), &expected);
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
        (dec!(100.00), "NZD", WeightSource::Native),
        (dec!(-100.00), "NZD", WeightSource::Price ),
    ]
)]
fn price_inference(src: &str, expected: Vec<(rust_decimal::Decimal, &str, WeightSource)>) {
    let result = parse_txn_and_balance(src);
    let expected = expected
        .into_iter()
        .map(|(num, cur, ws)| (num, cur.to_string(), ws))
        .collect::<Vec<_>>();
    assert_eq!(&result.unwrap(), &expected);
}
