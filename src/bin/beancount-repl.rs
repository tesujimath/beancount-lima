use beancount_lima::ledger::{self, register_types_and_functions, Ledger};
use std::path::PathBuf;
use steel::steel_vm::engine::Engine;
use steel_repl::run_repl;

fn main() -> Result<(), ledger::Error> {
    let flags = xflags::parse_or_exit! {
        /// Show allocations
        optional --show-allocations

        /// File to parse
        required path: PathBuf
    };

    let stderr = &std::io::stderr();

    match Ledger::parse_from(&flags.path, stderr) {
        Ok(ledger) => {
            let mut steel_engine = Engine::new();

            register_types_and_functions(&mut steel_engine);
            steel_engine
                .register_external_value("ledger", ledger)
                .unwrap(); // can't fail

            run_repl(steel_engine).map_err(ledger::Error::Io)
        }
        Err(e) => Err(e),
    }
}
