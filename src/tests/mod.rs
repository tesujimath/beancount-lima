use steel::steel_vm::engine::Engine;

use crate::{run_emitting_error, Error};

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
