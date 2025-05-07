use std::{fmt::Display, io, path::PathBuf};
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

const LIMA_PRELUDE: &str = "lima/prelude";

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

    if !flags.no_prelude {
        load_cog(&mut steel_engine, LIMA_PRELUDE)?;
    }
    if let Some(cog) = &flags.cog {
        load_cog(&mut steel_engine, cog)?;
    }

    if flags.batch {
        return Ok(());
    }

    run_repl(steel_engine).map_err(Error::Io)?;

    Ok(())
}

fn register(steel_engine: &mut Engine, ledger: Ledger) {
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

fn run_emitting_error<S>(
    steel_engine: &mut Engine,
    error_context: &str,
    input: S,
) -> Result<(), Error>
where
    S: AsRef<str>,
{
    let input = input.as_ref();
    if let Err(e) = steel_engine.run(input.to_string()) {
        e.emit_result(error_context, input);
        return Err(Error::Scheme);
    }

    Ok(())
}

fn load_cog<S>(steel_engine: &mut Engine, cog_name: S) -> Result<(), Error>
where
    S: AsRef<str>,
{
    let cog_name = cog_name.as_ref();
    let load_cog_command = format!("(require \"{}.scm\")", cog_name);
    run_emitting_error(steel_engine, cog_name, &load_cog_command)
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
pub(crate) mod types;
pub(crate) use types::*;
#[cfg(test)]
mod tests;
