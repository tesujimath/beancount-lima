use beancount_lima_booking::{is_supported_method, Booking};
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess,
};
use color_eyre::eyre::{eyre, Result, WrapErr};
use std::{io::Write, iter::once, path::Path};

use crate::{
    format::edn::write_booked_as_edn,
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
                    write_booked_as_edn(&directives, &options, out_w)
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

fn write_as_beancount<'a, W>(
    directives: &[Directive<'a>],
    _options: &parser::Options,
    mut out_w: W,
) -> Result<()>
where
    W: Write + Copy,
{
    for d in directives {
        writeln!(out_w, "{d}")?;
    }
    Ok(())
}
