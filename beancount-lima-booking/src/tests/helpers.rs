use beancount_parser_lima as parser;
use std::io::stderr;
use time::Date;
use tracing_subscriber::EnvFilter;

use crate::{book_with_residuals, Booking, BookingError, Bookings, Inventory};

const ANTE_TAG: &str = "ante";
const EX_TAG: &str = "ex";
const APPLY_TAG: &str = "apply";
const BOOKED_TAG: &str = "booked";
const AMBI_MATCHES_TAG: &str = "ambi-matches";
const AMBI_RESOLVED_TAG: &str = "ambi-resolved";
const REDUCED_TAG: &str = "reduced";
const PRINT_TAG: &str = "print";

pub(crate) fn booking_test_ok(source: &str, options: &str, method: Booking) {
    booking_test(source, options, method, None);
}

pub(crate) fn booking_test_err(source: &str, options: &str, method: Booking, err: BookingError) {
    booking_test(source, options, method, Some(err));
}

pub(crate) fn booking_test(
    source: &str,
    options: &str,
    method: Booking,
    expected_err: Option<BookingError>,
) {
    init_tracing();

    let source_with_options = format!("{options}\n{source}");
    let sources = parser::BeancountSources::from(source_with_options);
    let parser = parser::BeancountParser::new(&sources);
    let error_w = &stderr();

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

            if let Some((date, ante_postings)) = get_postings(&directives, ANTE_TAG) {
                let (
                    Bookings {
                        updated_inventory, ..
                    },
                    residuals,
                ) = book_with_residuals(
                    date,
                    &ante_postings,
                    &tolerance,
                    |_| None,
                    |_| Booking::Strict,
                )
                .unwrap();

                ante_inventory = updated_inventory;
            }

            println!("ante inventory {:?}", &ante_inventory);

            let (date, postings) =
                get_postings(&directives, APPLY_TAG).expect("missing apply tag in test data");

            let actual_inventory = match (
                book_with_residuals(
                    date,
                    &postings,
                    &tolerance,
                    |accname| ante_inventory.get(accname),
                    |_| method,
                ),
                expected_err,
            ) {
                (
                    Ok((
                        Bookings {
                            updated_inventory, ..
                        },
                        residuals,
                    )),
                    None,
                ) => updated_inventory,
                (Err(e), Some(expected_err)) => {
                    assert_eq!(&e, &expected_err);
                    return ();
                }
                (Ok(_), Some(_)) => panic!("unexpected success"),
                (Err(e), None) => panic!("unexpected failure {e}"),
            };

            let (date, postings) =
                get_postings(&directives, EX_TAG).expect("missing ex tag in test data");
            let (
                Bookings {
                    updated_inventory: expected_inventory,
                    ..
                },
                residuals,
            ) = book_with_residuals(date, &postings, &tolerance, |_| None, |_| Booking::Strict)
                .unwrap();

            assert_eq!(&actual_inventory, &expected_inventory);

            // TODO check booked
        }
    }
}

fn get_postings<'a>(
    directives: &'a [parser::Spanned<parser::Directive<'a>>],
    tag0: &'static str,
) -> Option<(Date, Vec<&'a parser::Spanned<parser::Posting<'a>>>)> {
    directives
        .iter()
        .filter(|d| d.metadata().tags().any(|tag| tag.item().as_ref() == tag0))
        .filter_map(|d| {
            if let parser::DirectiveVariant::Transaction(t) = d.variant() {
                Some((*d.date().item(), t.postings().collect::<Vec<_>>()))
            } else {
                None
            }
        })
        .next()
}

pub(crate) const NO_OPTIONS: &str = "";

fn init_tracing() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        let subscriber = tracing_subscriber::fmt()
            .with_env_filter(EnvFilter::from_default_env())
            .finish();
        tracing::subscriber::set_global_default(subscriber).unwrap();
    });
}
