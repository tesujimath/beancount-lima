// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess,
};
use color_eyre::eyre::{eyre, Result, WrapErr};
use std::{io::Write, iter::once, path::Path};
use steel::{gc::Gc, rvals::IntoSteelVal, SteelVal};

use crate::{
    config::LoaderConfig,
    loader::balancing::InferredTolerance,
    loader::{LoadError, LoadSuccess, Loader},
    prism::types as prism,
    prism::Ledger,
};

pub(crate) fn parse_from<W>(path: &Path, config: LoaderConfig, error_w: W) -> Result<Ledger>
where
    W: Write + Copy,
{
    let sources = BeancountSources::try_from(path).wrap_err(format!("failed to read {path:?}"))?;
    let parser = BeancountParser::new(&sources);

    let (builder, options) = match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options,
            plugins: _,
            warnings,
        }) => {
            sources.write_errors_or_warnings(error_w, warnings)?;
            let mut builder = Loader::new(&options, config);
            let inferred_tolerance = InferredTolerance::new(&options);

            for directive in directives {
                builder.directive(&directive, &inferred_tolerance);
            }

            let options = convert_parser_options(&options)
                .map(|(k, v)| (SteelVal::SymbolV(k.to_string().into()), v))
                .collect::<steel::HashMap<SteelVal, SteelVal>>();
            let options = Gc::new(options).into();

            Ok((builder, options))
        }

        Err(ParseError { errors, warnings }) => {
            sources.write_errors_or_warnings(error_w, errors)?;
            sources.write_errors_or_warnings(error_w, warnings)?;
            Err(eyre! {"parse error"})
        }
    }?;

    drop(parser);

    match builder.validate() {
        Ok(LoadSuccess {
            directives,
            warnings,
        }) => {
            if !warnings.is_empty() {
                sources.write_errors_or_warnings(error_w, warnings)?;
            }

            Ok(Ledger {
                sources: sources.into(),
                directives: directives.into(),
                options,
            })
        }
        Err(LoadError { errors, .. }) => {
            sources.write_errors_or_warnings(error_w, errors)?;
            Err(eyre!("builder error"))
        }
    }
}

/// Convert just those parser options that make sense to expose to Scheme.
/// TODO options
pub(crate) fn convert_parser_options(
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
