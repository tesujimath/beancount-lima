use steel::steel_vm::engine::Engine;

use crate::AlistItem;

#[derive(Default)]
pub(crate) struct OptionsBuilder {
    standalone: bool,
}

impl OptionsBuilder {
    pub(crate) fn standalone(&mut self) {
        self.standalone = true;
    }

    fn build(self) -> Vec<AlistItem> {
        self.standalone
            .then_some(("standalone", true).into())
            .into_iter()
            .collect::<Vec<AlistItem>>()
    }

    pub(crate) fn register(self, steel_engine: &mut Engine) {
        steel_engine
            .register_external_value("*ffi-options*", self.build())
            .unwrap(); // can't fail
    }
}
