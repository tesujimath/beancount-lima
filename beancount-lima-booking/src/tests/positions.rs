use rstest::rstest;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use time::macros::date;

use super::{positions_test, Booking};

#[rstest]
#[case(r#"
2025-04-01 txn
  Assets:Bank 10.50 USD            
  Assets:Bank  3.25 USD            
"#,
        Booking::Strict,
        &[("USD", dec!(13.75), None)],
    )]
#[case(r#"
2025-04-01 txn
  Assets:Bank -19.50 NZD            
  Assets:Bank   3.00 GBP            
  Assets:Bank  -4.00 GBP            
  Assets:Bank  10.21 CAD
"#,
        Booking::Strict,
        &[
            ("CAD", dec!(10.21), None),
            ("GBP", dec!(-1.00), None),
            ("NZD", dec!(-19.50), None),
        ],
    )]
fn test_positions_no_cost(
    #[case] source: &str,
    #[case] method: Booking,
    #[case] expected_positions: &[(
        &str,
        Decimal,
        Option<(time::Date, Decimal, &str, Option<&str>, bool)>,
    )],
) {
    positions_test(source, method, expected_positions)
}

#[rstest]
#[case(r#"
2025-04-01 txn
  Assets:Shares 1 MSFT                           ; look Ma, no cost, just testing ordering of not-at-cost with respect to at-cost
  Assets:Shares 5 HOOL { 150.00 NZD, 2025-04-01 }
  Assets:Shares 3 HOOL { 160.00 NZD, 2025-04-02 }
  Assets:Shares 1 HOOL { 150.00 NZD, 2025-04-01 }
  Assets:Shares 1 MSFT { 560.00 GBP, 2025-04-03 }
  Assets:Shares 1 MSFT { 260.00 USD, 2025-03-31 }
  Assets:Shares 2 MSFT                           ; look Ma, no cost, just testing ordering of not-at-cost with respect to at-cost
"#,
        Booking::Strict,
        &[
            ("HOOL", dec!(6), Some((date!(2025-04-01), dec!(150.00), "NZD", None, false))),
            ("HOOL", dec!(3), Some((date!(2025-04-02), dec!(160.00), "NZD", None, false))),
            ("MSFT", dec!(3), None),
            ("MSFT", dec!(1), Some((date!(2025-03-31), dec!(260.00), "USD", None, false))),
            ("MSFT", dec!(1), Some((date!(2025-04-03), dec!(560.00), "GBP", None, false))),
        ],
    )]
fn test_positions_cost_strict(
    #[case] source: &str,
    #[case] method: Booking,
    #[case] expected_positions: &[(
        &str,
        Decimal,
        Option<(time::Date, Decimal, &str, Option<&str>, bool)>,
    )],
) {
    positions_test(source, method, expected_positions)
}

#[rstest]
#[case(r#"
2025-04-01 txn
  Assets:Shares 1 MSFT
  Assets:Shares 1 MSFT { 560.00 GBP, 2025-04-03 }
  Assets:Shares 5 HOOL { 150.00 NZD, 2025-04-01 }
  Assets:Shares 3 HOOL { 160.00 NZD, 2025-04-02 }
  Assets:Shares 1 HOOL { 150.00 NZD, 2025-04-01 }
  Assets:Shares 1 MSFT { 260.00 USD, 2025-03-31 }
  Assets:Shares 2 MSFT                           ; look Ma, no cost, just testing ordering of not-at-cost with respect to at-cost
"#,
        Booking::None,
        &[
            ("HOOL", dec!(5), Some((date!(2025-04-01), dec!(150.00), "NZD", None, false))),
            ("HOOL", dec!(3), Some((date!(2025-04-02), dec!(160.00), "NZD", None, false))),
            ("HOOL", dec!(1), Some((date!(2025-04-01), dec!(150.00), "NZD", None, false))),
            ("MSFT", dec!(3), None),
            ("MSFT", dec!(1), Some((date!(2025-04-03), dec!(560.00), "GBP", None, false))),
            ("MSFT", dec!(1), Some((date!(2025-03-31), dec!(260.00), "USD", None, false))),
        ],
    )]
fn test_positions_cost_none(
    #[case] source: &str,
    #[case] method: Booking,
    #[case] expected_positions: &[(
        &str,
        Decimal,
        Option<(time::Date, Decimal, &str, Option<&str>, bool)>,
    )],
) {
    positions_test(source, method, expected_positions)
}
