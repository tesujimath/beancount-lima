use beancount_parser_lima as parser;
use hashbrown::HashMap;
use rust_decimal::Decimal;
use std::{collections::HashSet, io::stderr, iter::once};
use time::Date;
use tracing_subscriber::EnvFilter;

use crate::{
    book_with_residuals, is_supported_method, Booking, BookingError, Bookings, Interpolated,
    Inventory, Tolerance,
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

                    check_inventory_as_expected(actual_inventory, &directives, &tolerance);

                    check_postings_as_expected(interpolated_postings, &directives);
                }
            }

            // run a single tests for all combined, if any
            let apply_combined = get_postings(&directives, APPLY_COMBINED_TAG)
                .enumerate()
                .collect::<Vec<_>>();
            if !apply_combined.is_empty() {
                let mut actual_inventory = ante_inventory.clone().into();

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
                    }
                }

                check_inventory_as_expected(actual_inventory, &directives, &tolerance);

                // TODO check booked
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
    ) = book_with_residuals(date, &postings, tolerance, |_| None, |_| Booking::Strict).unwrap();

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
                    units: actual_units,
                    currency: actual_currency,
                    cost: actual_cost,
                    price: _actual_price, // TODO price comparison
                    ..
                } = actual_posting;
                if let Some(actual_cost) = actual_cost {
                    actual_cost
                        .into_currency_costs()
                        .filter_map(|(cur, pc)| {
                            // we filter out zero postings, since they're generally not included in the expected
                            (pc.units != Decimal::ZERO).then_some((
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
                    once((actual_units, actual_currency, None, None, None, None, false))
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
                if let Some(cost) = posting.cost_spec() {
                    let per_unit = cost.per_unit().map(|x| x.item().value());
                    let currency = cost.currency().map(|x| x.item());
                    let date = cost.date().map(|x| x.item());
                    let label = cost.label().map(|x| *x.item());
                    let merge = cost.merge();
                    (
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

fn init_tracing() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        let subscriber = tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::from_default_env())
            .finish();
        tracing::subscriber::set_global_default(subscriber).unwrap();
    });
}
