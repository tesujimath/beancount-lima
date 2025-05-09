use std::{fs::read_to_string, path::Path};
use steel::steel_vm::engine::Engine;

use crate::{run_emitting_error, Error};

fn load_cog_path<P>(steel_engine: &mut Engine, cog_path: P) -> Result<(), Error>
where
    P: AsRef<Path>,
{
    let cog_path = cog_path.as_ref();
    let cog_content = read_to_string(cog_path).unwrap();
    run_emitting_error(
        steel_engine,
        cog_path.to_string_lossy().as_ref(),
        &cog_content,
    )
}

fn report_test_failures(steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
    run_emitting_error(
        steel_engine,
        "",
        format!(
            r#"
                (require "steel/tests/unit-test.scm")
                (when (not (empty? (hash-get (get-test-stats) 'failures))) (error! "test failures in {}"))
            "#,
            cog_relpath
        ),
    )
}

mod beancount_tests;
mod cog_tests;
