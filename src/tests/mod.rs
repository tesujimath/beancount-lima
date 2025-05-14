use std::{fs::read_to_string, path::Path};

use steel::steel_vm::engine::Engine;

use crate::{run_emitting_error_discarding_result, Error};

fn report_test_failures(steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
    run_emitting_error_discarding_result(
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
/// Load the specific cog from its path
fn load_cog_path<P>(steel_engine: &mut Engine, cog_path: P) -> Result<(), Error>
where
    P: AsRef<Path>,
{
    let cog_path = cog_path.as_ref();
    let cog_content = read_to_string(cog_path).map_err(Error::Io)?;
    let cog_path = cog_path.to_string_lossy();
    run_emitting_error_discarding_result(steel_engine, cog_path.as_ref(), &cog_content)
}

mod beancount_tests;
mod cog_tests;
