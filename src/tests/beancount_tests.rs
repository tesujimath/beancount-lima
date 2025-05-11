#![cfg(test)]

use std::path::Path;
use steel::steel_vm::engine::Engine;
use test_generator::test_resources;

use crate::{load_cog_path, tests::report_test_failures, CogPaths, Ledger};

#[test_resources("tests/beancount/**/*.beancount")]
fn beancount_tests(beancount_relpath: &str) {
    let cog_relpath = format!(
        "{}.scm",
        beancount_relpath.strip_suffix(".beancount").unwrap(),
    );

    let ledger = Ledger::parse_from(Path::new(beancount_relpath), &std::io::stderr()).unwrap();

    let mut steel_engine = Engine::new();
    ledger.register(&mut steel_engine);

    let cog_paths = CogPaths::from_env();

    cog_paths.set_steel_search_path(&mut steel_engine);

    load_cog_path(&mut steel_engine, &cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, &cog_relpath).unwrap();
}
