use color_eyre::eyre::Result;
use steel::{steel_vm::engine::Engine, SteelVal};

use crate::run_emitting_error;

pub(crate) fn get_config_string(
    steel_engine: &mut Engine,
    path: &[&str],
) -> Result<Option<String>> {
    get_config_val(steel_engine, path).map(|v| {
        if let Some(SteelVal::StringV(v)) = v {
            Some(v.to_string())
        } else {
            None
        }
    })
}

pub(crate) fn get_config_bool(steel_engine: &mut Engine, path: &[&str]) -> Result<Option<bool>> {
    get_config_val(steel_engine, path).map(|v| {
        if let Some(SteelVal::BoolV(v)) = v {
            Some(v)
        } else {
            None
        }
    })
}

pub(crate) fn get_config_val(steel_engine: &mut Engine, path: &[&str]) -> Result<Option<SteelVal>> {
    run_emitting_error(
        steel_engine,
        "get_config_value",
        format!(
            r#"
    (config-value-or-default '({}) '() *config*)
    "#,
            path.join(" "),
        ),
    )
    .map(|result| result.into_iter().next())
}

#[derive(Default, Debug)]
pub(crate) struct LoaderConfig {
    pub(crate) balance_rollup: bool,
}

impl LoaderConfig {
    pub(crate) fn get(steel_engine: &mut Engine) -> Result<Self> {
        let balance_rollup = get_config_bool(steel_engine, &["balance-rollup"])?.unwrap_or(false);
        Ok(Self { balance_rollup })
    }
}
