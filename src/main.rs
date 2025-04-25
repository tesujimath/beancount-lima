use std::{fmt::Display, io, path::PathBuf};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_repl::run_repl;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

fn main() -> Result<(), Error> {
    let flags = xflags::parse_or_exit! {
        /// Don't load the prelude
        optional --no-prelude

        /// Exit after loading and validating all files
        optional --batch

        /// Beancount ledger path
        required beancount_path: PathBuf

        /// additional cog to load
        optional cog: String
    };

    let ledger = Ledger::parse_from(&flags.beancount_path, &std::io::stderr())?;

    let mut steel_engine = Engine::new();

    register(&mut steel_engine, ledger);

    set_search_path(&mut steel_engine);

    load_cogs(&mut steel_engine, !flags.no_prelude, &flags.cog)?;

    if flags.batch {
        return Ok(());
    }

    run_repl(steel_engine).map_err(Error::Io)?;

    Ok(())
}

fn register(steel_engine: &mut Engine, ledger: Ledger) {
    steel_engine.register_fn("tabulate", crate::tabulate::tabulate);

    types::register_types_with_engine(steel_engine);

    steel_engine
        .register_external_value("*ffi-ledger*", ledger)
        .unwrap(); // can't fail
}

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

fn load_cogs(
    steel_engine: &mut Engine,
    load_prelude: bool,
    cli_cog: &Option<String>,
) -> Result<(), Error> {
    if load_prelude {
        // load ledger prelude
        let prelude = "lima/prelude";
        let load_prelude_command = format!("(require \"{}.scm\")", prelude);
        if let Err(e) = steel_engine.run(load_prelude_command.clone()) {
            e.emit_result(prelude, &load_prelude_command);
            return Err(Error::Scheme);
        }
    }

    // load additional CLI cog if any
    if let Some(cog) = cli_cog {
        let load_cog_command = format!("(require \"{}.scm\")", cog);
        if let Err(e) = steel_engine.run(load_cog_command.clone()) {
            e.emit_result(cog, &load_cog_command);
            return Err(Error::Scheme);
        }
    }

    Ok(())
}

#[derive(Debug)]
pub(crate) enum Error {
    Io(io::Error),
    Parser,
    Builder,
    Scheme,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
            Builder => f.write_str("builder errors"),
            Scheme => f.write_str("error in Scheme"),
        }
    }
}

pub(crate) mod ledger;
pub(crate) mod tabulate;
pub(crate) mod types;
pub(crate) use types::*;
