#![cfg(test)]

use std::{ffi::OsStr, path::PathBuf};

use steel::steel_vm::engine::Engine;

use crate::{load_cog, register, set_search_path, Ledger, LIMA_PRELUDE};

#[test]
fn beancount_tests() {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let tests_dir = manifest_dir.join("tests");
    for entry in tests_dir.read_dir().unwrap() {
        let entry = entry.unwrap();
        let entry_name: PathBuf = entry.file_name().into();
        if entry.file_type().unwrap().is_file()
            && entry_name.extension() == Some(OsStr::new("beancount"))
        {
            let cog_name = entry_name.file_stem().unwrap().to_string_lossy();

            let beancount_path = tests_dir.join(&entry_name);
            println!(
                "found {:?} for {}.scm and {:?}",
                &entry_name, cog_name, &beancount_path
            );
            let ledger = Ledger::parse_from(&beancount_path, &std::io::stderr()).unwrap();

            let mut steel_engine = Engine::new();

            register(&mut steel_engine, ledger);

            set_search_path(&mut steel_engine);
            steel_engine.add_search_directory(tests_dir.clone());

            load_cog(&mut steel_engine, LIMA_PRELUDE).unwrap();
            load_cog(&mut steel_engine, &cog_name).unwrap();
        }
    }
}
