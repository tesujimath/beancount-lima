use std::{fmt::Display, fs::read_to_string, io, path::PathBuf};
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

const LIMA_PRELUDE: &str = "lima/prelude";

struct CogPaths(Vec<PathBuf>);

impl CogPaths {
    fn from_env() -> Self {
        let mut paths = Vec::default();

        if let Ok(path) = std::env::var(BEANCOUNT_LIMA_COGPATH) {
            for path in path.split(":") {
                if path.is_empty() {
                    continue;
                }
                let dir = PathBuf::from(path);
                if dir.is_dir() {
                    paths.push(dir);
                } else {
                    eprintln!(
                        "warning: ${} contains directory {} which does not exist",
                        BEANCOUNT_LIMA_COGPATH, path
                    )
                }
            }
        }

        Self(paths)
    }

    fn set_steel_search_path(&self, steel_engine: &mut Engine) {
        for path in &self.0 {
            steel_engine.add_search_directory(path.clone());
        }
    }

    fn load_cog(&self, steel_engine: &mut Engine, cog_name: &str) -> Result<(), Error> {
        let cog_basename = format!("{}.scm", cog_name);
        for path in &self.0 {
            let cog_path = path.join(&cog_basename);
            if let Ok(cog_content) = read_to_string(&cog_path) {
                return run_emitting_error(
                    steel_engine,
                    cog_path.to_string_lossy().as_ref(),
                    cog_content,
                );
            }
        }
        Err(Error::NoSuchCog(cog_name.to_string()))
    }
}

fn main() -> Result<(), Error> {
    let flags = xflags::parse_or_exit! {
        /// Don't load the prelude
        optional --no-prelude

        /// Exit after loading and validating all files
        optional --batch

        /// Set test mode
        optional --test

        /// Beancount ledger path
        required beancount_path: PathBuf

        /// additional cog to load
        optional cog: String
    };

    let ledger = Ledger::parse_from(&flags.beancount_path, &std::io::stderr())?;

    let mut steel_engine = Engine::new();

    register(&mut steel_engine, ledger);

    let cog_paths = CogPaths::from_env();
    cog_paths.set_steel_search_path(&mut steel_engine);

    if flags.test {
        set_test_mode(&mut steel_engine).unwrap();
    }

    if let Some(cog) = &flags.cog {
        cog_paths.load_cog(&mut steel_engine, cog)?;
    }

    if flags.batch {
        return Ok(());
    }

    // the prelude is only auto-loaded for the REPL,
    // all Scheme files must load it explicitly
    if !flags.no_prelude {
        cog_paths.load_cog(&mut steel_engine, LIMA_PRELUDE)?;
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
fn set_test_mode(steel_engine: &mut Engine) -> Result<(), Error> {
    run_emitting_error(steel_engine, "", "(set-test-mode!)")
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

#[derive(Debug)]
pub(crate) enum Error {
    Io(io::Error),
    Parser,
    Builder,
    Scheme,
    NoSuchCog(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
            Builder => f.write_str("builder errors"),
            Scheme => f.write_str("error in Scheme"),
            NoSuchCog(name) => write!(f, "no such cog {} in ${}", name, BEANCOUNT_LIMA_COGPATH),
        }
    }
}

pub(crate) mod ledger;
pub(crate) mod types;
pub(crate) use types::*;
#[cfg(test)]
mod tests;
