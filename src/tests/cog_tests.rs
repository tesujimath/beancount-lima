#![cfg(test)]

use std::{ffi::OsStr, path::PathBuf};
use steel::steel_vm::engine::Engine;
use walkdir::WalkDir;

use crate::{load_cog, load_cog_path, register, set_search_path, Ledger, LIMA_PRELUDE};

#[test]
fn cog_tests() {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let cogs_dir = manifest_dir.join("cogs");

    for entry in WalkDir::new(&cogs_dir) {
        let entry = entry.unwrap();
        let entry_name: PathBuf = entry.file_name().into();
        if entry.file_type().is_file() && entry_name.extension() == Some(OsStr::new("scm")) {
            let entry_relpath = entry.path().strip_prefix(&cogs_dir).unwrap();
            println!(
                "found {:?} at {:?} relpath {:?}",
                &entry_name,
                entry.path(),
                entry_relpath
            );
            let empty_ledger = Ledger::empty();

            let mut steel_engine = Engine::new();

            register(&mut steel_engine, empty_ledger);

            set_search_path(&mut steel_engine);
            steel_engine.add_search_directory(cogs_dir.clone());

            load_cog(&mut steel_engine, LIMA_PRELUDE).unwrap();
            let entry_relpath = entry_relpath.to_string_lossy();
            load_cog_path(&mut steel_engine, entry_relpath.as_ref()).unwrap();
        }
    }
}
