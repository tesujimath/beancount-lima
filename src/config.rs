use color_eyre::eyre::Result;
use steel::{steel_vm::engine::Engine, SteelVal};

use crate::run_emitting_error;

pub(crate) fn get_config_string(
    steel_engine: &mut Engine,
    path: &[&str],
) -> Result<Option<String>> {
    run_emitting_error(
        steel_engine,
        "get_config_value",
        format!(
            r#"
    (config-value-or-empty '({}) *config*)
    "#,
            path.join(" "),
        ),
    )
    .map(|result| {
        result.first().and_then(|s| {
            if let SteelVal::StringV(s) = s {
                Some(s.to_string())
            } else {
                None
            }
        })
    })
}
