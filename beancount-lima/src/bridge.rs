use beancount_lima_booking::{is_supported_method, Booking};
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess,
};
use color_eyre::eyre::{eyre, Result, WrapErr};
use std::{io::Write, iter::once, path::Path};
use steel::{gc::Gc, rvals::IntoSteelVal, SteelVal};

use crate::{
    loader::{InferredTolerance, LoadError, LoadSuccess, Loader},
    plugins::InternalPlugins,
    prism::Prism,
};

pub(crate) fn load_from<W>(path: &Path, error_w: W) -> Result<Prism>
where
    W: Write + Copy,
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

                    let prism_directives =
                        conversions::convert_directives(directives, &internal_plugins);

                    let options = convert_parser_options(&options)
                        .map(|(k, v)| (SteelVal::SymbolV(k.to_string().into()), v))
                        .collect::<steel::HashMap<SteelVal, SteelVal>>();
                    let options = Gc::new(options).into();

                    drop(parser);

                    Ok(Prism {
                        sources: sources.into(),
                        directives: prism_directives.into(),
                        options,
                    })
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

/// Convert just those parser options that make sense to expose to Scheme.
/// TODO options
fn convert_parser_options(
    options: &parser::Options<'_>,
) -> impl Iterator<Item = (&'static str, SteelVal)> {
    once((
        "name_assets",
        options
            .account_type_name(parser::AccountType::Assets)
            .to_string()
            .into_steelval()
            .unwrap(),
    ))
    .chain(once((
        "name_liabilities",
        options
            .account_type_name(parser::AccountType::Liabilities)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_equity",
        options
            .account_type_name(parser::AccountType::Equity)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_income",
        options
            .account_type_name(parser::AccountType::Income)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_expenses",
        options
            .account_type_name(parser::AccountType::Expenses)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
}

mod conversions;
