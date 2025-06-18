use config::get_config_string;
use import::{Context, Group};
use ledger::Ledger;
use std::{
    fmt::Display,
    io::{self},
    path::PathBuf,
};
use steel::{steel_vm::engine::Engine, SteelVal};
use steel_repl::run_repl;
use tracing_subscriber::EnvFilter;

const BEANCOUNT_LIMA_COGPATH: &str = "BEANCOUNT_LIMA_COGPATH";

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Don't run the REPL
    #[clap(long)]
    batch: bool,

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
        /// Files to import
        import_files: Vec<PathBuf>,

        /// Base ledger for import
        #[arg(long)]
        ledger: Option<PathBuf>,

        /// Cog to load for import
        #[arg(long, value_name = "COG")]
        using: Option<String>,
    },

    /// Test one or more cogs
    Test {
        /// Files to import
        cogs: Vec<String>,
    },
}

struct CogPaths(Vec<PathBuf>);

impl CogPaths {
    fn from_env() -> Self {
        let mut paths = Vec::default();

        if let Some(cog_dir) = user_cog_dir() {
            paths.push(cog_dir);
        }

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

// return user cog dir if it exists
fn user_cog_dir() -> Option<PathBuf> {
    let xdg_dirs = xdg::BaseDirectories::with_prefix("beancount-lima");
    xdg_dirs
        .get_data_home()
        .map(|path| path.join("cogs"))
        .and_then(|cog_dir| {
            if cog_dir.is_dir() {
                Some(cog_dir)
            } else {
                None
            }
        })
}

/// Load the cog using Steel's search path
fn load_cog(steel_engine: &mut Engine, cog_relpath: &str) -> Result<(), Error> {
    let run_command = format!(r#"(require "{}")"#, cog_relpath);
    run_emitting_error_discarding_result(steel_engine, cog_relpath, &run_command)
}

pub(crate) fn create_engine() -> Engine {
    let mut steel_engine = Engine::new();
    register_types(&mut steel_engine);

    let cog_paths = CogPaths::from_env();
    cog_paths.set_steel_search_path(&mut steel_engine);

    steel_engine
}

fn main() -> Result<(), Error> {
    let error_w = &std::io::stderr();

    let mut steel_engine = create_engine();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    load_cog(&mut steel_engine, "lima/lib/base-config.scm")?;
    load_cog(&mut steel_engine, "lima/config.scm")?;

    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Count { ledger, using }) => {
            let ledger = Ledger::parse_from(ledger, error_w)?;
            ledger.register(&mut steel_engine);

            if let Some(cog) = using {
                load_cog(&mut steel_engine, &format!("lima/count/{}.scm", cog))?;
            }
        }

        Some(Commands::Import {
            import_files,
            ledger: ledger_path,
            using,
        }) => {
            let txnid_key =
                get_config_string(&mut steel_engine, &["import", "txnid-key"], "txnid")?;
            let txnid2_key =
                get_config_string(&mut steel_engine, &["import", "txnid2-key"], "txnid2")?;
            let payee2_key =
                get_config_string(&mut steel_engine, &["import", "payee2-key"], "payee2")?;
            let narration2_key = get_config_string(
                &mut steel_engine,
                &["import", "narration2-key"],
                "narration2",
            )?;

            let ledger = if let Some(ledger) = ledger_path.as_ref() {
                Ledger::parse_from(ledger, error_w)?
            } else {
                Ledger::empty()
            };
            ledger.register(&mut steel_engine);

            let context = if let Some(ledger) = ledger_path.as_ref() {
                Context::parse_from(
                    ledger,
                    vec![txnid_key, txnid2_key],
                    payee2_key,
                    narration2_key,
                    error_w,
                )?
            } else {
                Context::default()
            };

            let import = Group::parse_from(import_files.as_slice(), context, error_w)?;
            import.register(&mut steel_engine);

            load_cog(
                &mut steel_engine,
                &format!(
                    "lima/import/{}.scm",
                    using.as_ref().map_or("default", |s| s.as_str())
                ),
            )?;
        }

        Some(Commands::Test { cogs }) => {
            set_test_mode(&mut steel_engine).unwrap();

            for cog in cogs {
                let cog_relpath = format!("lima/{}.scm", cog);
                load_cog(&mut steel_engine, &cog_relpath)?;
                report_test_failures(&mut steel_engine, &cog_relpath)?;
            }

            return Ok(());
        }

        None => (),
    };

    if cli.batch {
        return Ok(());
    }

    // the optional prelude is only auto-loaded for the REPL,
    if !cli.no_prelude {
        load_cog(&mut steel_engine, "lima/prelude.scm")?;
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
            steel_engine.raise_error(e);
            // if let Some(msg) = steel_engine.raise_error_to_string(e) {
            //     eprintln!("{}", msg);
            // }
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

fn register_types(steel_engine: &mut Engine) {
    types::register_types(steel_engine);
    ledger::register_types(steel_engine);
    import::register_types(steel_engine);
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

pub(crate) mod config;
pub(crate) mod import;
pub(crate) mod ledger;
pub(crate) mod types;
pub(crate) use types::*;
#[cfg(test)]
mod tests;
