use steel::steel_vm::engine::Engine;

use crate::AlistItem;

/// Register options, each of which may be name=value or just name,
/// where values are strings, and bare names register as boolean true.
pub(crate) fn register_cli_options(steel_engine: &mut Engine, options: Vec<String>) {
    let options = options
        .into_iter()
        .map(|opt| {
            if let Some((k, v)) = opt.split_once("=") {
                (k, v.to_string()).into()
            } else {
                // bool option, set true
                (opt, true).into()
            }
        })
        .collect::<Vec<AlistItem>>();

    steel_engine
        .register_external_value("*ffi-cli-options*", options)
        .unwrap(); // can't fail
}
