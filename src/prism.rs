// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::BeancountSources;
use steel::{
    gc::{Gc, Shared},
    rvals::SteelHashMap,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal,
};
use steel_derive::Steel;

#[derive(Clone, Debug, Steel)]
pub(crate) struct Ledger {
    pub(crate) sources: CustomShared<BeancountSources>,
    pub(crate) directives: Shared<Vec<Directive>>,
    pub(crate) options: SteelHashMap,
}

impl Ledger {
    /// Empty ledger
    pub(crate) fn empty() -> Self {
        Ledger {
            sources: BeancountSources::from("").into(),
            directives: Vec::default().into(),
            options: Gc::new(steel::HashMap::default()).into(),
        }
    }

    fn sources(&self) -> CustomShared<BeancountSources> {
        self.sources.clone()
    }

    fn directives(&self) -> Vec<Directive> {
        (*self.directives).clone()
    }

    fn options(&self) -> SteelVal {
        SteelVal::HashMapV(self.options.clone())
    }

    pub(crate) fn register(self, steel_engine: &mut Engine) {
        steel_engine
            .register_external_value("*sources*", self.sources())
            .unwrap(); // can't fail
        steel_engine
            .register_external_value("*directives*", self.directives())
            .unwrap(); // can't fail
        steel_engine
            .register_external_value("*options*", self.options())
            .unwrap(); // can't fail
    }
}

fn write_ffi_error(sources: &CustomShared<BeancountSources>, error: WrappedError) {
    tracing::debug!("write_ffi_error");

    sources
        .write_errors_or_warnings(&std::io::stderr(), vec![error.as_ref().clone()])
        .unwrap();
}

fn write_ffi_errors(sources: &CustomShared<BeancountSources>, errors: Vec<WrappedError>) {
    tracing::debug!("write_ffi_errors");

    sources
        .write_errors_or_warnings(
            &std::io::stderr(),
            errors
                .iter()
                .map(|e| e.as_ref().clone())
                .collect::<Vec<_>>(),
        )
        .unwrap();
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    inventory::register_types(steel_engine);
    tabulate::register(steel_engine);
    types::register_types(steel_engine);
    steel_engine.register_fn("sources-write-ffi-error", write_ffi_error);
    steel_engine.register_fn("sources-write-ffi-errors", write_ffi_errors);
}

pub(crate) mod args;
pub(crate) mod booking;
pub(crate) mod format;
pub(crate) mod inventory;
pub(crate) mod tabulate;
pub(crate) mod types;
use types::*;
