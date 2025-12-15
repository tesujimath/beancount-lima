use beancount_lima_booking::{is_supported_method, Booking};
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess,
};
use color_eyre::eyre::{eyre, Result, WrapErr};
use std::{
    io::{BufWriter, Write},
    iter::once,
    path::Path,
};

use crate::{
    loader::{Directive, InferredTolerance, LoadError, LoadSuccess, Loader},
    plugins::InternalPlugins,
};

pub(crate) fn load_from<W1, W2>(path: &Path, out_w: W1, error_w: W2) -> Result<()>
where
    W1: Write + Copy,
    W2: Write + Copy,
{
    let sources = BeancountSources::try_from(path).wrap_err(format!("failed to read {path:?}"))?;
    let parser = BeancountParser::new(&sources);

    match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options,
            plugins,
            mut warnings,
        }) => {
            let internal_plugins = plugins.iter().collect::<InternalPlugins>();
            let inferred_tolerance = InferredTolerance::new(&options);

            let default_booking = Booking::default();
            let default_booking_option = if let Some(booking_method) = options.booking_method() {
                let booking = Into::<Booking>::into(*booking_method.item());
                if is_supported_method(booking) {
                    booking
                } else {
                    warnings.push(booking_method.warning(format!(
                        "Unsupported booking method, falling back to {default_booking}"
                    )));
                    default_booking
                }
            } else {
                default_booking
            };

            sources.write_errors_or_warnings(error_w, warnings)?;

            match Loader::new(
                default_booking_option,
                inferred_tolerance,
                &options,
                &internal_plugins,
            )
            .collect(&directives)
            {
                Ok(LoadSuccess {
                    directives,
                    warnings,
                }) => {
                    if !warnings.is_empty() {
                        sources.write_errors_or_warnings(error_w, warnings)?;
                    }

                    // TODO both beancount and EDN format
                    write_as_edn(&directives, out_w)
                    // write_as_beancount(&directives, out_w)
                }
                Err(LoadError { errors, .. }) => {
                    sources.write_errors_or_warnings(error_w, errors)?;
                    Err(eyre!("builder error"))
                }
            }
        }

        Err(ParseError { errors, warnings }) => {
            sources.write_errors_or_warnings(error_w, errors)?;
            sources.write_errors_or_warnings(error_w, warnings)?;
            Err(eyre! {"parse error"})
        }
    }
}

fn write_as_beancount<'a, W>(directives: &[Directive<'a>], mut out_w: W) -> Result<()>
where
    W: Write + Copy,
{
    for d in directives {
        writeln!(out_w, "{}", d)?;
    }
    Ok(())
}

fn write_as_edn<'a, W>(directives: &[Directive<'a>], out_w: W) -> Result<()>
where
    W: Write + Copy,
{
    let mut buffered_out_w = BufWriter::new(out_w);

    writeln!(buffered_out_w, "{}", crate::format::edn::VECTOR_BEGIN)?;
    for d in directives {
        writeln!(buffered_out_w, "{}", crate::format::edn::Edn(d))?;
    }
    writeln!(buffered_out_w, "{}", crate::format::edn::VECTOR_END)?;
    Ok(())
}

/// Convert just those parser options that make sense to expose to Scheme.
/// TODO options
fn convert_parser_options(
    options: &parser::Options<'_>,
) -> impl Iterator<Item = (&'static str, String)> {
    once((
        "name_assets",
        options
            .account_type_name(parser::AccountType::Assets)
            .to_string(),
    ))
    .chain(once((
        "name_liabilities",
        options
            .account_type_name(parser::AccountType::Liabilities)
            .to_string(),
    )))
    .chain(once((
        "name_equity",
        options
            .account_type_name(parser::AccountType::Equity)
            .to_string(),
    )))
    .chain(once((
        "name_income",
        options
            .account_type_name(parser::AccountType::Income)
            .to_string(),
    )))
    .chain(once((
        "name_expenses",
        options
            .account_type_name(parser::AccountType::Expenses)
            .to_string(),
    )))
}
