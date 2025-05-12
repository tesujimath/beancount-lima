use steel::{steel_vm::engine::Engine, SteelVal};

use crate::{run_emitting_error, Error};

pub(crate) fn get_config_string(
    steel_engine: &mut Engine,
    path: &[&str],
    default: &str,
) -> Result<String, Error> {
    run_emitting_error(
        steel_engine,
        "get_config_value",
        format!(
            r#"
    (config-value-or-default '({}) "{}" *config*)
    "#,
            path.join(" "),
            default,
        ),
    )
    .map(|result| {
        println!("config value returned {:?}", result);
        result
            .first()
            .and_then(|s| {
                if let SteelVal::StringV(s) = s {
                    Some(s.to_string())
                } else {
                    None
                }
            })
            .unwrap_or(default.to_string())
    })
}
