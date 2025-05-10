#![cfg(test)]

use steel::steel_vm::engine::Engine;
use test_generator::test_resources;

use super::{load_cog_path, report_test_failures};
use crate::{set_test_mode, CogPaths, Ledger};

#[test_resources("cogs/**/tests/*.scm")]
fn cog_tests(cog_relpath: &str) {
    let empty_ledger = Ledger::empty();
    let mut steel_engine = Engine::new();

    // TODO don't do this?
    empty_ledger.register(&mut steel_engine);

    let cog_paths = CogPaths::from_env();

    cog_paths.set_steel_search_path(&mut steel_engine);

    set_test_mode(&mut steel_engine).unwrap();
    load_cog_path(&mut steel_engine, cog_relpath).unwrap();
    report_test_failures(&mut steel_engine, cog_relpath).unwrap();
}
