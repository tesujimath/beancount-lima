use crate::{
    format::edn::write_import_as_edn,
    import::{Context, Import},
    loader::Loader,
};
use color_eyre::eyre::Result;
use std::path::PathBuf;
use tracing_subscriber::EnvFilter;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Extensible comma-separated options, each split on `=` and passed as strings, or as bools if no `=`
    #[clap(short, value_delimiter = ',')]
    options: Vec<String>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Calculate all the bookings
    Book {
        /// Beancount ledger
        ledger: PathBuf,
    },

    /// Import from external CSV or OFX file
    Import {
        /// Beancount ledger
        #[arg(long)]
        ledger: Option<PathBuf>,

        /// Files to import
        import_files: Vec<PathBuf>,
    },
}

fn main() -> Result<()> {
    let out_w = &std::io::stdout();
    let error_w = &std::io::stderr();

    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let cli = Cli::parse();

    match &cli.command {
        Command::Book { ledger } => book::load_from(ledger, out_w, error_w),

        Command::Import {
            ledger,
            import_files,
        } => {
            let context = if let Some(ledger) = ledger.as_ref() {
                Some(Context::load_from(
                    ledger,
                    vec![TXNID_KEY.to_string(), TXNID2_KEY.to_string()],
                    PAYEE2_KEY.to_string(),
                    NARRATION2_KEY.to_string(),
                    error_w,
                )?)
            } else {
                None
            };

            let import = Import::parse_from(import_files.as_slice(), context, error_w)?;
            write_import_as_edn(&import, out_w)
        }
    }
}

const TXNID_KEY: &str = "txnid";
const TXNID2_KEY: &str = "txnid2";
const PAYEE2_KEY: &str = "payee2";
const NARRATION2_KEY: &str = "narration2";

pub(crate) mod book;
pub(crate) mod format;
pub(crate) mod import;
pub(crate) mod loader;
pub(crate) mod options;
pub(crate) mod plugins;
