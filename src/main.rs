use std::path::PathBuf;
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

fn set_search_path(steel_engine: &mut Engine) {
    // setup search path for cogs
    if let Ok(path) = std::env::var(BEANCOUNT_LIMA_COGPATH) {
        for path in path.split(":") {
            if path.is_empty() {
                continue;
            }
            let dir = PathBuf::from(path);
            if dir.is_dir() {
                steel_engine.add_search_directory(dir);
            } else {
                eprintln!(
                    "warning: ${} contains directory {} which does not exist",
                    BEANCOUNT_LIMA_COGPATH, path
                )
            }
        }
    }
}

fn main() -> Result<(), ledger::Error> {
    let flags = xflags::parse_or_exit! {
        /// Exit after loading and validating all files
        optional --batch

        /// Beancount ledger path
        required beancount_path: PathBuf

        /// additional Steel cogs to load
        repeated cog: String
    };

    let stderr = &std::io::stderr();

    let ledger = Ledger::parse_from(&flags.beancount_path, stderr)?;

    let mut steel_engine = Engine::new();

    set_search_path(&mut steel_engine);

    register_types_and_functions(&mut steel_engine);
    steel_engine
        .register_external_value("*ffi-ledger*", ledger)
        .unwrap(); // can't fail

    // // TODO make this a function
    // load ledger prelude
    let prelude = "lima/prelude.scm";
    let load_prelude_command = format!("(require \"{}\")", prelude);
    if let Err(e) = steel_engine.run(load_prelude_command.clone()) {
        e.emit_result(prelude, &load_prelude_command);
        return Err(ledger::Error::Scheme);
    }

    // load additional scheme files, if any
    for cog in &flags.cog {
        let load_cog_command = format!("(require \"{}\")", cog);
        if let Err(e) = steel_engine.run(load_cog_command.clone()) {
            e.emit_result(cog, &load_cog_command);
            return Err(ledger::Error::Scheme);
        }
    }

    if flags.batch {
        return Ok(());
    }

    run_repl(steel_engine).map_err(ledger::Error::Io)?;

    Ok(())
}

pub mod ledger;
use ledger::*;
pub mod tabulate;
