#![cfg(test)]

use steel::steel_vm::engine::Engine;
use test_generator::test_resources;

use super::{load_cog_path, report_test_failures};
use crate::{register, CogPaths, Ledger};

#[test_resources("cogs/**/tests/*.scm")]
fn cog_tests(cog_relpath: &str) {
    let empty_ledger = Ledger::empty();
    let mut steel_engine = Engine::new();

    register(&mut steel_engine, empty_ledger);

    let cog_paths = CogPaths::from_env();

    cog_paths.set_steel_search_path(&mut steel_engine);

    load_cog_path(&mut steel_engine, cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, cog_relpath).unwrap();
}
