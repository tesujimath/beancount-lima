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

fn booking_test(source: &str, options: &str, method: Booking, expected_err: Option<BookingError>) {
    init_tracing();

    let source_with_options = format!("{options}\n{source}");
    let sources = parser::BeancountSources::from(source_with_options);
    let parser = parser::BeancountParser::new(&sources);
    let error_w = &stderr();

    // TODO support other booking methods
    if method != Booking::Strict {
        panic!("Failing for now because unsupported booking method Booking::{method}");
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

            for (i_apply, (date, postings, apply_string)) in
                get_postings(&directives, APPLY_TAG).enumerate()
            {
                let actual_inventory = match (
                    book_with_residuals(
                        date,
                        &postings,
                        &tolerance,
                        |accname| ante_inventory.get(accname),
                        |_| method,
                    ),
                    expected_err.as_ref(),
                ) {
                    (
                        Ok((
                            Bookings {
                                updated_inventory, ..
                            },
                            _residuals,
                        )),
                        None,
                    ) => updated_inventory,
                    (Err(e), Some(expected_err)) => {
                        assert_eq!(&e, expected_err);
                        continue;
                    }
                    (Ok(_), Some(_)) => panic!("unexpected success at {i_apply}\n{apply_string}"),
                    (Err(e), None) => panic!("unexpected failure {e} at {i_apply}\n{apply_string}"),
                };

                let (date, postings, _) = get_postings(&directives, EX_TAG)
                    .next()
                    .expect("missing ex tag in test data");
                let (
                    Bookings {
                        updated_inventory: expected_inventory,
                        ..
                    },
                    _residuals,
                ) = book_with_residuals(date, &postings, &tolerance, |_| None, |_| Booking::Strict)
                    .unwrap();

                assert_eq!(&actual_inventory, &expected_inventory);

                // TODO check booked
            }
        }
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
