use steel::{gc::Gc, rvals::IntoSteelVal, steel_vm::engine::Engine, SteelVal};

/// Register options as a Steel HashMap, each of which may be name=value or just name,
/// where values are strings, and bare names register as boolean true.
pub(crate) fn register_args(steel_engine: &mut Engine, args: Vec<String>) {
    let args = args
        .into_iter()
        .map(|arg| {
            if let Some((k, v)) = arg.split_once("=") {
                (k.to_string(), v.to_string().into_steelval())
            } else {
                // bool option, set true
                (arg, true.into_steelval())
            }
        })
        .map(|(k, v)| (SteelVal::SymbolV(k.into()), v.unwrap()))
        .collect::<steel::HashMap<SteelVal, SteelVal>>();

    steel_engine
        .register_external_value("*args*", SteelVal::HashMapV(Gc::new(args).into()))
        .unwrap(); // can't fail
}
