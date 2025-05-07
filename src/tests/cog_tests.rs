#![cfg(test)]

use std::path::PathBuf;
use steel::steel_vm::engine::Engine;
use test_generator::test_resources;

use super::{load_cog_path, report_test_failures, set_test_mode};
use crate::{load_cog, register, set_search_path, Ledger, LIMA_PRELUDE};

#[test_resources("cogs/**/*.scm")]
fn cog_tests(cog_relpath: &str) {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let cogs_dir = manifest_dir.join("cogs");

    let empty_ledger = Ledger::empty();
    let mut steel_engine = Engine::new();

    register(&mut steel_engine, empty_ledger);

    set_search_path(&mut steel_engine);
    steel_engine.add_search_directory(cogs_dir.clone());

    set_test_mode(&mut steel_engine).unwrap();

    load_cog(&mut steel_engine, LIMA_PRELUDE).unwrap();
    load_cog_path(&mut steel_engine, cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, cog_relpath).unwrap();
}
