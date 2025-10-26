#![cfg(test)]

use rstest::rstest;
use std::path::PathBuf;

use super::load_cog_path;
use crate::{create_engine, report_test_failures, set_test_mode};

#[rstest]
fn cog_tests(#[files("tests/cogs/**/*.scm")] cog_relpath: PathBuf) {
    let mut steel_engine = create_engine();

    set_test_mode(&mut steel_engine).unwrap();
    load_cog_path(&mut steel_engine, &cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, &cog_relpath).unwrap();
}
