use beancount_parser_lima::{self as parser, DirectiveVariant};
use hashbrown::HashMap;
use rust_decimal::Decimal;
use std::{collections::HashSet, io::stderr, iter::once};
use time::Date;
use tracing_subscriber::EnvFilter;

use crate::{
    book_with_residuals, is_supported_method, Booking, BookingError, Bookings, Cost, Interpolated,
    Inventory, Position, Positions, Tolerance,
};

const ANTE_TAG: &str = "ante";
const EX_TAG: &str = "ex";
const APPLY_TAG: &str = "apply";
const APPLY_COMBINED_TAG: &str = "apply-combined";
const BOOKED_TAG: &str = "booked";
// unused:
// const AMBI_MATCHES_TAG: &str = "ambi-matches";
// const AMBI_RESOLVED_TAG: &str = "ambi-resolved";
// const REDUCED_TAG: &str = "reduced";
// const PRINT_TAG: &str = "print";

pub(crate) fn booking_test_ok(source: &str, method: Booking) {
    booking_test(source, method, None);
}

pub(crate) fn booking_test_err(source: &str, method: Booking, err: BookingError) {
    booking_test(source, method, Some(err));
}

fn booking_test(source: &str, method: Booking, expected_err: Option<BookingError>) {
    let sources = parser::BeancountSources::from(source);
    let parser = parser::BeancountParser::new(&sources);
    let error_w = &stderr();

    if !is_supported_method(method) {
        panic!("Failing for now because Booking::{method} is unsupported");
    }

    match parser.parse() {
        Err(parser::ParseError { errors, .. }) => {
            sources.write_errors_or_warnings(error_w, errors).unwrap();
            panic!("unexpected parse failure in test data");
        }

        Ok(parser::ParseSuccess {
            directives,
            options,
            ..
        }) => {
            let tolerance = &options;
            let mut ante_inventory = Inventory::default();

            if let Some((date, ante_postings, _)) = get_postings(&directives, ANTE_TAG).next() {
                let (
                    Bookings {
                        updated_inventory, ..
                    },
                    _residuals,
                ) = book_with_residuals(date, &ante_postings, &tolerance, |_| None, |_| method)
                    .unwrap();

                ante_inventory = updated_inventory;
            }

            init_tracing();

            // run a separate test for each posting tagged with apply
            for (i_apply, (date, postings, apply_string)) in
                get_postings(&directives, APPLY_TAG).enumerate()
            {
                let mut actual_inventory = ante_inventory.clone().into();

                tracing::debug!("book_with_residuals {:?}", &postings);
                let location = format!("{} {}", ordinal(i_apply), APPLY_TAG);
                if let Some(Bookings {
                    interpolated_postings,
                    updated_inventory,
                    ..
                }) = book_and_check_error(
                    date,
                    &postings,
                    &mut actual_inventory,
                    &tolerance,
                    method,
                    expected_err.as_ref(),
                    &location,
                    &apply_string,
                ) {
                    tracing::debug!("updating test inventory with {:?}", &updated_inventory);
                    for (acc, positions) in updated_inventory {
                        actual_inventory.insert(acc, positions);
                    }

                    check_inventory_as_expected(actual_inventory, &directives, &tolerance, method);

                    check_postings_as_expected(interpolated_postings, &directives);
                }
            }

            // run a single tests for all combined, if any
            let apply_combined = get_postings(&directives, APPLY_COMBINED_TAG)
                .enumerate()
                .collect::<Vec<_>>();
            if !apply_combined.is_empty() {
                let mut actual_inventory = ante_inventory.clone().into();
                let mut actual_postings = Vec::default();

                for (i_apply, (date, postings, apply_string)) in apply_combined {
                    tracing::debug!("book_with_residuals {:?}", &postings);
                    let location = format!("{} {}", ordinal(i_apply), APPLY_TAG);
                    if let Some(Bookings {
                        interpolated_postings,
                        updated_inventory,
                        ..
                    }) = book_and_check_error(
                        date,
                        &postings,
                        &mut actual_inventory,
                        &tolerance,
                        method,
                        expected_err.as_ref(),
                        &location,
                        &apply_string,
                    ) {
                        tracing::debug!("updating test inventory with {:?}", &updated_inventory);
                        for (acc, positions) in updated_inventory {
                            actual_inventory.insert(acc, positions);
                        }

                        actual_postings.extend(interpolated_postings);
                    }
                }

                check_inventory_as_expected(actual_inventory, &directives, &tolerance, method);

                check_postings_as_expected(actual_postings, &directives);
            }
        }
    }
}

fn ordinal(i: usize) -> String {
    format!(
        "{}{}",
        i,
        match i {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        }
    )
}

fn book_and_check_error<'a, 'b, T>(
    date: Date,
    postings: &[&'a parser::Spanned<parser::Posting<'a>>],
    inventory: &mut Inventory<&'a str, time::Date, Decimal, parser::Currency<'a>, &'a str>,
    tolerance: &'b T,
    method: Booking,
    expected_err: Option<&BookingError>,
    location_in_case_of_error: &str,
    source_in_case_of_error: &str,
) -> Option<Bookings<&'a parser::Spanned<parser::Posting<'a>>>>
where
    T: Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
{
    match (
        book_with_residuals(
            date,
            postings,
            tolerance,
            |accname| inventory.get(accname),
            |_| method,
        ),
        expected_err,
    ) {
        (Ok((bookings, _residuals)), None) => Some(bookings),
        (Err(e), Some(expected_err)) => {
            assert_eq!(&e, expected_err);
            None
        }
        (Ok(_), Some(_)) => {
            panic!("unexpected success at {location_in_case_of_error}\n{source_in_case_of_error}")
        }
        (Err(e), None) => panic!(
            "unexpected failure {e} at {location_in_case_of_error}\n{source_in_case_of_error}"
        ),
    }
}

fn check_inventory_as_expected<'a, 'b, T>(
    actual_inventory: Inventory<&'a str, time::Date, Decimal, parser::Currency<'a>, &'a str>,
    directives: &'a [parser::Spanned<parser::Directive<'a>>],
    tolerance: &'b T,
    method: Booking,
) where
    T: Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
{
    let (date, postings, _) = get_postings(directives, EX_TAG)
        .next()
        .expect("missing ex tag in test data");
    let (
        Bookings {
            updated_inventory: expected_inventory,
            ..
        },
        _residuals,
    ) = book_with_residuals(date, &postings, tolerance, |_| None, |_| method).unwrap();

    // since we can't build an expected inventory with an empty account, we remove all such from the result before comparison
    let actual_inventory = Into::<Inventory<_, _, _, _, _>>::into(
        actual_inventory
            .into_iter()
            .filter_map(|(account, positions)| {
                (!positions.is_empty()).then_some((account, positions))
            })
            .collect::<HashMap<_, _>>(),
    );

    assert_eq!(&actual_inventory, &expected_inventory);
}

fn check_postings_as_expected<'a>(
    actual_postings: Vec<
        Interpolated<
            &'a parser::Spanned<parser::Posting<'a>>,
            time::Date,
            Decimal,
            parser::Currency<'a>,
            &'a str,
        >,
    >,
    directives: &'a [parser::Spanned<parser::Directive<'a>>],
) {
    if let Some((_date, expected_postings, _)) = get_postings(directives, BOOKED_TAG).next() {
        let actual = actual_postings
            .into_iter()
            .flat_map(|actual_posting| {
                let Interpolated {
                    posting: actual_posting,
                    units: actual_units,
                    currency: actual_currency,
                    cost: actual_cost,
                    price: _actual_price, // TODO price comparison
                    ..
                } = actual_posting;
                let actual_account = actual_posting.account().item().as_ref();
                if let Some(actual_cost) = actual_cost {
                    actual_cost
                        .into_currency_costs()
                        .filter_map(|(cur, pc)| {
                            // we filter out zero postings, since they're generally not included in the expected
                            (pc.units != Decimal::ZERO).then_some((
                                actual_account,
                                pc.units,
                                actual_currency,
                                Some(pc.per_unit),
                                Some(cur),
                                Some(pc.date),
                                pc.label,
                                pc.merge,
                            ))
                        })
                        .collect::<HashSet<_>>()
                } else if actual_units != Decimal::ZERO {
                    once((
                        actual_account,
                        actual_units,
                        actual_currency,
                        None,
                        None,
                        None,
                        None,
                        false,
                    ))
                    .collect::<HashSet<_>>()
                } else {
                    HashSet::default()
                }
            })
            .collect::<HashSet<_>>();

        let expected = expected_postings
            .into_iter()
            .map(|spanned| {
                let posting = spanned.item();
                let expected_account = posting.account().item().as_ref();
                if let Some(cost) = posting.cost_spec() {
                    let per_unit = cost.per_unit().map(|x| x.item().value());
                    let currency = cost.currency().map(|x| x.item());
                    let date = cost.date().map(|x| x.item());
                    let label = cost.label().map(|x| *x.item());
                    let merge = cost.merge();
                    (
                        expected_account,
                        posting.amount().unwrap().item().value(),
                        *posting.currency().unwrap().item(),
                        per_unit,
                        currency.copied(),
                        date.copied(),
                        label,
                        merge,
                    )
                } else {
                    (
                        expected_account,
                        posting.amount().unwrap().item().value(),
                        *posting.currency().unwrap().item(),
                        None,
                        None,
                        None,
                        None,
                        false,
                    )
                }
            })
            .collect::<HashSet<_>>();

        assert_eq!(actual, expected);
    }
}

fn get_postings<'a>(
    directives: &'a [parser::Spanned<parser::Directive<'a>>],
    tag0: &'static str,
) -> impl Iterator<Item = (Date, Vec<&'a parser::Spanned<parser::Posting<'a>>>, String)> {
    directives
        .iter()
        .filter(move |d| d.metadata().tags().any(|tag| tag.item().as_ref() == tag0))
        .filter_map(|d| {
            if let parser::DirectiveVariant::Transaction(t) = d.variant() {
                Some((
                    *d.date().item(),
                    t.postings().collect::<Vec<_>>(),
                    d.to_string(),
                ))
            } else {
                None
            }
        })
}

pub(crate) fn positions_test(
    source: &str,
    method: Booking,
    expected_positions: &[(
        &str,
        Decimal,
        Option<(time::Date, Decimal, &str, Option<&str>, bool)>,
    )],
) {
    let sources = parser::BeancountSources::from(source);
    let parser = parser::BeancountParser::new(&sources);
    let error_w = &stderr();

    if !is_supported_method(method) {
        panic!("Failing for now because Booking::{method} is unsupported");
    }

    match parser.parse() {
        Err(parser::ParseError { errors, .. }) => {
            sources.write_errors_or_warnings(error_w, errors).unwrap();
            panic!("unexpected parse failure in test data");
        }

        Ok(parser::ParseSuccess { directives, .. }) => {
            if directives.len() != 1 {
                panic!("test requires precisely 1 directive");
            }
            let directive = directives[0].item();

            if let DirectiveVariant::Transaction(txn) = directive.variant() {
                let positions = txn
                    .postings()
                    .map(|p| {
                        let amount = p.amount().expect("posting amount is required");
                        let units = amount.value();
                        let currency = p.currency().expect("posting currency is required").item();

                        if let Some(cost_spec) = p.cost_spec().as_ref().map(|cs| cs.item()) {
                            let cost_per_unit = cost_spec
                                .per_unit()
                                .expect("cost per-unit is required")
                                .value();
                            let cost_currency = cost_spec
                                .currency()
                                .expect("cost currency is required")
                                .item();
                            let cost_date =
                                *cost_spec.date().expect("cost date is required").item();
                            let cost_label = cost_spec.label().map(|label| *label.item());
                            let merge = cost_spec.merge();
                            let cost = Cost {
                                date: cost_date,
                                per_unit: cost_per_unit,
                                currency: cost_currency,
                                label: cost_label,
                                merge,
                            };

                            Position {
                                currency,
                                units,
                                cost: Some(cost),
                            }
                        } else {
                            Position {
                                currency,
                                units,
                                cost: None,
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                init_tracing();

                let mut actual_positions =
                    Positions::<Date, Decimal, &parser::Currency, &str>::default();
                for Position {
                    currency,
                    units,
                    cost,
                } in positions
                {
                    actual_positions.accumulate(units, currency, cost, method);
                }

                let actual_positions = actual_positions
                    .iter()
                    .map(|p| {
                        (
                            p.currency.as_ref(),
                            p.units,
                            p.cost.as_ref().map(|cost| {
                                (
                                    cost.date,
                                    cost.per_unit,
                                    cost.currency.as_ref(),
                                    cost.label.as_ref().map(|label| *label),
                                    cost.merge,
                                )
                            }),
                        )
                    })
                    .collect::<Vec<_>>();
                assert_eq!(&actual_positions, expected_positions);
            }
        }
    }
}

fn init_tracing() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        let subscriber = tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::from_default_env())
            .finish();
        tracing::subscriber::set_global_default(subscriber).unwrap();
    });
}
