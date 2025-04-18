use std::{fs::read_to_string, path::PathBuf};
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

fn main() -> Result<(), ledger::Error> {
    let flags = xflags::parse_or_exit! {
        /// Exit after loading and validating all files
        optional --check

        /// Beancount ledger path
        required beancount_path: PathBuf

        /// Steel Scheme files to load
        repeated scheme_path: PathBuf
    };

    let stderr = &std::io::stderr();

    let ledger = Ledger::parse_from(&flags.beancount_path, stderr)?;

    let mut steel_engine = Engine::new();

    register_types_and_functions(&mut steel_engine);
    steel_engine
        .register_external_value("*ffi-ledger*", ledger)
        .unwrap(); // can't fail

    // load ledger cog
    let ledger_cog = include_str!("../cogs/ledger.scm");
    if let Err(e) = steel_engine.run(ledger_cog) {
        e.emit_result("cogs/ledger.scm", ledger_cog);
        return Err(ledger::Error::Scheme);
    }

    // load additional scheme files, if any
    for scheme_path in &flags.scheme_path {
        let content = read_to_string(scheme_path).map_err(ledger::Error::Io)?;
        if let Err(e) = steel_engine.run(content) {
            e.emit_result(scheme_path.to_string_lossy().as_ref(), ledger_cog);
            return Err(ledger::Error::Scheme);
        }
    }

    if flags.check {
        return Ok(());
    }

    run_repl(steel_engine).map_err(ledger::Error::Io)?;

    Ok(())
}

pub mod ledger;
use ledger::*;
