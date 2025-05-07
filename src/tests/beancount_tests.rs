#![cfg(test)]

use std::path::PathBuf;
use steel::steel_vm::engine::Engine;
use test_generator::test_resources;

use crate::{
    load_cog, register, set_search_path,
    tests::{report_test_failures, require_test_helpers},
    Ledger, LIMA_PRELUDE,
};

#[test_resources("tests/*.beancount")]
fn beancount_tests(beancount_relpath: &str) {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let tests_dir = manifest_dir.join("tests");
    let beancount_path = manifest_dir.join(beancount_relpath);
    let cog_name = beancount_relpath.strip_suffix(".beancount").unwrap();

    let ledger = Ledger::parse_from(&beancount_path, &std::io::stderr()).unwrap();

    let mut steel_engine = Engine::new();
    register(&mut steel_engine, ledger);

    set_search_path(&mut steel_engine);
    steel_engine.add_search_directory(tests_dir.clone());

    load_cog(&mut steel_engine, LIMA_PRELUDE).unwrap();

    require_test_helpers(&mut steel_engine).unwrap();
    load_cog(&mut steel_engine, cog_name).unwrap();
    report_test_failures(&mut steel_engine, cog_name).unwrap();
}
