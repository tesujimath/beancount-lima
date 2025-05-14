use config::get_config_string;
use import::{context::ImportContext, Imported};
use ledger::Ledger;
use std::{fmt::Display, io, path::PathBuf};
use steel::{steel_vm::engine::Engine, SteelVal};
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
        ledger: PathBuf,

        /// Cog to load for count
        #[arg(long, value_name = "COG")]
        using: Option<String>,
    },

    /// Import from external CSV or OFX file
    Import {
        /// File to import
        import_file: PathBuf,

        /// Base ledger for import
        #[arg(long)]
        ledger: Option<PathBuf>,

        /// Cog to load for import
        #[arg(long, value_name = "COG")]
        using: Option<String>,
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
}

/// Load the cog using Steel's search path
fn load_cog(steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
    let run_command = format!(r#"(require "{}")"#, cog_relpath);
    run_emitting_error_discarding_result(steel_engine, cog_relpath, &run_command)
}

fn main() -> Result<(), Error> {
    let mut steel_engine = Engine::new();
    let error_w = &std::io::stderr();

    let cog_paths = CogPaths::from_env();
    cog_paths.set_steel_search_path(&mut steel_engine);
    load_cog(&mut steel_engine, "lima/base-config.scm")?;
    load_cog(&mut steel_engine, "lima/config.scm")?;

    let cli = Cli::parse();
    let prelude_cog = match &cli.command {
        Some(Commands::Count { ledger, using }) => {
            let ledger = Ledger::parse_from(ledger, error_w)?;
            ledger.register(&mut steel_engine);

            if let Some(cog) = using {
                load_cog(&mut steel_engine, &format!("lima/count/{}.scm", cog))?;
            }

            Some("lima/count/prelude.scm")
        }

        Some(Commands::Import {
            import_file,
            ledger,
            using,
        }) => {
            let txnid_key =
                get_config_string(&mut steel_engine, &["import", "tnxid-key"], "txnid")?;
            let import_context = if let Some(ledger) = ledger {
                ImportContext::parse_from(ledger, txnid_key, error_w)?
            } else {
                ImportContext::default()
            };
            let import = Imported::parse_from(import_file, import_context, error_w)?;
            import.register(&mut steel_engine);

            load_cog(
                &mut steel_engine,
                &format!(
                    "lima/import/{}.scm",
                    using.as_ref().map_or("default", |s| s.as_str())
                ),
            )?;

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
            load_cog(&mut steel_engine, prelude_cog)?;
        }
    }

    run_repl(steel_engine).map_err(Error::Io)?;

    Ok(())
}

fn set_test_mode(steel_engine: &mut Engine) -> Result<(), Error> {
    run_emitting_error_discarding_result(steel_engine, "", "(set-test-mode!)")
}

fn run_emitting_error<S>(
    steel_engine: &mut Engine,
    error_context: &str,
    input: S,
) -> Result<Vec<SteelVal>, Error>
where
    S: AsRef<str>,
{
    let input = input.as_ref();
    match steel_engine.run(input.to_string()) {
        Ok(result) => Ok(result),
        Err(e) => {
            match e
                .span()
                .and_then(|span| span.source_id)
                .and_then(|source_id| {
                    match (
                        steel_engine.get_path_for_source_id(&source_id),
                        steel_engine.get_source(&source_id),
                    ) {
                        (Some(path), Some(content)) => Some((path, content)),
                        _ => None,
                    }
                }) {
                Some((path, content)) => {
                    e.emit_result(path.to_string_lossy().as_ref(), content.as_ref());
                }
                None => {
                    e.emit_result(error_context, input);
                }
            }
            Err(Error::Scheme)
        }
    }
}

fn run_emitting_error_discarding_result<S>(
    steel_engine: &mut Engine,
    error_context: &str,
    input: S,
) -> Result<(), Error>
where
    S: AsRef<str>,
{
    run_emitting_error(steel_engine, error_context, input).map(|_result| ())
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

pub(crate) mod config;
pub(crate) mod import;
pub(crate) mod ledger;
pub(crate) mod types;
pub(crate) use types::*;
#[cfg(test)]
mod tests;
