use import::Imported;
use ledger::Ledger;
use std::{
    fmt::Display,
    fs::read_to_string,
    io,
    path::{Path, PathBuf},
};
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Don't run the REPL
    #[clap(long)]
    batch: bool,

    /// Set test mode
    #[clap(long)]
    test: bool,

    /// Don't load the Scheme prelude
    #[clap(long)]
    no_prelude: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Count beans in a REPL
    Count {
        /// Beancount ledger
        #[clap(value_name = "FILE")]
        path: PathBuf,

        /// Initial cog to load
        cog: Option<String>,
    },

    Import {
        /// File to import
        #[clap(value_name = "FILE")]
        path: PathBuf,

        /// Cog to load for import
        importer: String,
    },
}

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

    /// Locate the file for the cog by looking in all our paths, and load it in Steel.
    /// First cog found wins.
    fn load_cog(&self, steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
        for search_dir in &self.0 {
            let cog_path = search_dir.join(cog_relpath);
            match load_cog_path(steel_engine, &cog_path) {
                Ok(_) => return Ok(()),
                Err(Error::Io(_)) => continue,
                Err(e) => return Err(e),
            }
        }
        Err(Error::Cli(format!(
            "no such cog {} in ${}",
            cog_relpath, BEANCOUNT_LIMA_COGPATH
        )))
    }
}

fn load_cog_path<P>(steel_engine: &mut Engine, cog_path: P) -> Result<(), Error>
where
    P: AsRef<Path>,
{
    let cog_path = cog_path.as_ref();
    let cog_content = read_to_string(cog_path).map_err(Error::Io)?;
    run_emitting_error(
        steel_engine,
        cog_path.to_string_lossy().as_ref(),
        &cog_content,
    )
}

fn main() -> Result<(), Error> {
    let mut steel_engine = Engine::new();

    let cog_paths = CogPaths::from_env();
    cog_paths.set_steel_search_path(&mut steel_engine);

    let cli = Cli::parse();
    let prelude_cog = match &cli.command {
        Some(Commands::Count {
            path: beancount_path,
            cog,
        }) => {
            let ledger = Ledger::parse_from(beancount_path, &std::io::stderr())?;
            ledger.register(&mut steel_engine);

            if let Some(cog) = cog {
                cog_paths.load_cog(&mut steel_engine, &format!("lima/count/{}.scm", cog))?;
            }

            Some("lima/count/prelude.scm")
        }

        Some(Commands::Import {
            path: import_path,
            importer,
        }) => {
            let import = Imported::parse_from(import_path, &std::io::stderr())?;
            import.register(&mut steel_engine);

            cog_paths.load_cog(&mut steel_engine, &format!("lima/import/{}.scm", importer))?;

            Some("lima/import/prelude.scm")
        }

        None => None,
    };
    if cli.test {
        set_test_mode(&mut steel_engine).unwrap();
    }

    if cli.batch {
        return Ok(());
    }

    // the optional prelude is only auto-loaded for the REPL,
    // all Scheme files must load it explicitly
    if let Some(prelude_cog) = prelude_cog {
        if !cli.no_prelude {
            cog_paths.load_cog(&mut steel_engine, prelude_cog)?;
        }
    }

    run_repl(steel_engine).map_err(Error::Io)?;

    Ok(())
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

pub(crate) fn register_types_with_engine(steel_engine: &mut Engine) {
    Account::register_with_engine(steel_engine);
    Amount::register_with_engine(steel_engine);
    Date::register_with_engine(steel_engine);
    Decimal::register_with_engine(steel_engine);
    Ledger::register_with_engine(steel_engine);
    Posting::register_with_engine(steel_engine);
    Imported::register_with_engine(steel_engine);
}

#[derive(Debug)]
pub(crate) enum Error {
    Io(io::Error),
    Csv(csv::Error),
    ImportFormat(String),
    Parser,
    Builder,
    Scheme,
    Cli(String),
    NotYetImplemented(&'static str),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            Io(e) => e.fmt(f),
            Csv(e) => e.fmt(f),
            ImportFormat(e) => e.fmt(f),
            Parser => f.write_str("parser error"),
            Builder => f.write_str("builder errors"),
            Scheme => f.write_str("error in Scheme"),
            Cli(e) => f.write_str(e),
            NotYetImplemented(feature) => write!(f, "{} not yet implemented", feature),
        }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Error::Io(value)
    }
}

pub(crate) mod import;
pub(crate) mod ledger;
pub(crate) mod types;
pub(crate) use types::*;
#[cfg(test)]
mod tests;
