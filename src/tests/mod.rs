use color_eyre::eyre::Result;
use std::{fs::read_to_string, path::Path};
use steel::steel_vm::engine::Engine;

use crate::run_emitting_error_discarding_result;

/// Load the specific cog from its path
fn load_cog_path<P>(steel_engine: &mut Engine, cog_path: P) -> Result<()>
where
    P: AsRef<Path>,
{
    let cog_path = cog_path.as_ref();
    let cog_content = read_to_string(cog_path)?;
    let cog_path = cog_path.to_string_lossy();
    run_emitting_error_discarding_result(steel_engine, cog_path.as_ref(), &cog_content)
}

mod beancount_tests;
mod cog_tests;
