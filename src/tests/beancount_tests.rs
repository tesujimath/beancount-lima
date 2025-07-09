#![cfg(test)]

use std::path::Path;
use test_generator::test_resources;

use crate::{create_engine, report_test_failures, Ledger, OptionsBuilder};

use super::load_cog_path;

#[test_resources("tests/beancount/**/*.beancount")]
fn beancount_tests(beancount_relpath: &str) {
    let cog_relpath = format!(
        "{}.scm",
        beancount_relpath.strip_suffix(".beancount").unwrap(),
    );

    let ledger = Ledger::parse_from(Path::new(beancount_relpath), &std::io::stderr()).unwrap();

    let mut steel_engine = create_engine();

    ledger.register(&mut steel_engine);
    OptionsBuilder::default().register(&mut steel_engine);

    load_cog_path(&mut steel_engine, &cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, &cog_relpath).unwrap();
}
