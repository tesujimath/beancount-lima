use crate::{
    args::register_args,
    config::{get_config_string, LedgerBuilderConfig},
    import::{Context, Import},
    ledger::Ledger,
};
use color_eyre::eyre::{eyre, Result};
use std::path::PathBuf;
use steel::{steel_vm::engine::Engine, SteelVal};
use steel_repl::run_repl;
use tracing_subscriber::EnvFilter;

const LIMA_COGPATH: &str = "LIMA_COGPATH";

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Beancount ledger, if any, overrides value from config
    #[arg(long)]
    ledger: Option<PathBuf>,

    /// Don't load the Scheme prelude
    #[clap(long)]
    no_prelude: bool,

    /// Extensible comma-separated options, each split on `=` and passed as strings, or as bools if no `=`
    #[clap(short, value_delimiter = ',')]
    options: Vec<String>,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Evaluate arbitrary cog then exit
    Report { cog: String },

    /// Import from external CSV or OFX file
    Import {
        /// Run the REPL instead of import and exit
        #[clap(long)]
        repl: bool,

        /// Files to import
        import_files: Vec<PathBuf>,
    },

    /// Test one or more cogs
    Test {
        /// Scheme files to load
        scheme_files: Vec<String>,
    },
}

struct CogPaths(Vec<PathBuf>);

impl CogPaths {
    fn from_env() -> Self {
        let mut paths = Vec::default();

        if let Some(cog_dir) = user_cog_dir() {
            paths.push(cog_dir);
        }

        if let Ok(path) = std::env::var(LIMA_COGPATH) {
            for path in path.split(":") {
                if path.is_empty() {
                    continue;
                }
                let dir = PathBuf::from(path);
                if dir.is_dir() {
                    paths.push(dir);
                } else {
                    eprintln!(
                        "warning: ${LIMA_COGPATH} contains directory {path} which does not exist"
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
fn load_cog<S>(steel_engine: &mut Engine, cog_relpath: S) -> Result<()>
where
    S: AsRef<str>,
{
    let cog_relpath = cog_relpath.as_ref();
    let run_command = format!(r#"(require "{cog_relpath}")"#);
    run_emitting_error_discarding_result(steel_engine, cog_relpath, &run_command)
}

pub(crate) fn create_engine() -> Engine {
    let mut steel_engine = Engine::new();
    register_types(&mut steel_engine);

    let cog_paths = CogPaths::from_env();
    cog_paths.set_steel_search_path(&mut steel_engine);

    steel_engine
}

fn main() -> Result<()> {
    let error_w = &std::io::stderr();

    let mut steel_engine = create_engine();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    load_cog(&mut steel_engine, "lima/lib/base-config.scm")?;
    load_cog(&mut steel_engine, "lima/config.scm")?;

    let cli = Cli::parse();
    register_args(&mut steel_engine, cli.options);

    let ledger_path = cli
        .ledger
        .or(get_config_string(&mut steel_engine, &["ledger"])?.map(PathBuf::from));
    let ledger = if let Some(ledger) = ledger_path.as_ref() {
        Ledger::parse_from(
            ledger,
            LedgerBuilderConfig::get(&mut steel_engine)?,
            error_w,
        )?
    } else {
        Ledger::empty()
    };
    ledger.register(&mut steel_engine);

    match &cli.command {
        None => (),

        Some(Command::Report { cog }) => {
            load_cog(&mut steel_engine, format!("lima/reports/{}.scm", &cog))?;
            return Ok(());
        }

        Some(Command::Import { repl, import_files }) => {
            let txnid_key = get_config_string(&mut steel_engine, &["import", "txnid-key"])?
                .unwrap_or("txnid".to_string());
            let txnid2_key = get_config_string(&mut steel_engine, &["import", "txnid2-key"])?
                .unwrap_or("txnid2".to_string());
            let payee2_key = get_config_string(&mut steel_engine, &["import", "payee2-key"])?
                .unwrap_or("payee2".to_string());
            let narration2_key =
                get_config_string(&mut steel_engine, &["import", "narration2-key"])?
                    .unwrap_or("narration2".to_string());

            let context = if let Some(ledger) = ledger_path.as_ref() {
                Some(Context::parse_from(
                    ledger,
                    vec![txnid_key, txnid2_key],
                    payee2_key,
                    narration2_key,
                    error_w,
                )?)
            } else {
                None
            };

            let import = Import::parse_from(import_files.as_slice(), context, error_w)?;
            import.register(&mut steel_engine);

            if *repl {
                load_cog(&mut steel_engine, "lima/lib/import/prelude.scm")?;
            } else {
                load_cog(&mut steel_engine, "lima/lib/import/cmd.scm")?;
                return Ok(());
            }
        }

        Some(Command::Test { scheme_files }) => {
            if let Some(ledger_path) = ledger_path.as_ref() {
                let ledger = Ledger::parse_from(
                    ledger_path,
                    LedgerBuilderConfig::get(&mut steel_engine)?,
                    error_w,
                )?;
                ledger.register(&mut steel_engine);
            };

            set_test_mode(&mut steel_engine).unwrap();

            for scheme_file in scheme_files {
                load_cog(&mut steel_engine, scheme_file)?;
                report_test_failures(&mut steel_engine, scheme_file)?;
            }

            return Ok(());
        }
    };

    // the optional prelude is only auto-loaded for the REPL,
    if !cli.no_prelude {
        load_cog(&mut steel_engine, "lima/lib/prelude.scm")?;
    }

    run_repl(steel_engine)?;

    Ok(())
}

fn set_test_mode(steel_engine: &mut Engine) -> Result<()> {
    run_emitting_error_discarding_result(steel_engine, "", "(set-test-mode!)")
}

fn run_emitting_error<S>(
    steel_engine: &mut Engine,
    error_context: &str,
    input: S,
) -> Result<Vec<SteelVal>>
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
            Err(eyre!("scheme error"))
        }
    }
}

fn run_emitting_error_discarding_result<S>(
    steel_engine: &mut Engine,
    error_context: &str,
    input: S,
) -> Result<()>
where
    S: AsRef<str>,
{
    run_emitting_error(steel_engine, error_context, input).map(|_result| ())
}

fn register_types(steel_engine: &mut Engine) {
    types::register_types(steel_engine);
    ledger::register_types(steel_engine);
    import::register_types(steel_engine);
    inventory::register_types(steel_engine);
}

fn report_test_failures(steel_engine: &mut Engine, cog_relpath: &str) -> Result<()> {
    run_emitting_error_discarding_result(
        steel_engine,
        "",
        format!(
            r#"
                (require "steel/tests/unit-test.scm")
                (when (not (empty? (hash-get (get-test-stats) 'failures))) (error! "test failures in {cog_relpath}"))
            "#
        ),
    )
}

pub(crate) mod args;
pub(crate) mod config;
pub(crate) mod format;
pub(crate) mod import;
pub(crate) mod inventory;
pub(crate) mod ledger;
pub(crate) mod shared;
pub(crate) mod steel_decimal;
pub(crate) mod steely;
pub(crate) mod tabulate;
pub(crate) mod types;

#[cfg(test)]
mod tests;
