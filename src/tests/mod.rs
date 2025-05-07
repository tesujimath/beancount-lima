use steel::steel_vm::engine::Engine;

use crate::{run_emitting_error, Error};

fn load_cog_path<S>(steel_engine: &mut Engine, cog_path: S) -> Result<(), Error>
where
    S: AsRef<str>,
{
    let cog_path = cog_path.as_ref();
    let load_cog_command = format!("(require \"{}\")", cog_path);
    run_emitting_error(steel_engine, cog_path, &load_cog_command)
}

fn require_unit_test(steel_engine: &mut Engine) -> Result<(), Error> {
    run_emitting_error(
        steel_engine,
        "",
        r#"(require "steel/tests/unit-test.scm" (for-syntax "steel/tests/unit-test.scm"))"#,
    )
}

fn set_test_mode(steel_engine: &mut Engine) -> Result<(), Error> {
    run_emitting_error(steel_engine, "", "(set-test-mode!)")
}

fn report_test_failures(steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
    run_emitting_error(
        steel_engine,
        "",
        format!(
            r#"(when (not (empty? (hash-get (get-test-stats) 'failures))) (error! "test failures in {}"))"#,
            cog_relpath
        ),
    )
}

mod beancount_tests;
mod cog_tests;
