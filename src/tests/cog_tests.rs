#![cfg(test)]

use test_generator::test_resources;

use super::load_cog_path;
use crate::{create_engine, report_test_failures, set_test_mode};

#[test_resources("tests/cogs/**/*.scm")]
fn cog_tests(cog_relpath: &str) {
    let mut steel_engine = create_engine();

    set_test_mode(&mut steel_engine).unwrap();
    load_cog_path(&mut steel_engine, cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, cog_relpath).unwrap();
}
