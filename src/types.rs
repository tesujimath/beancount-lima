// TODO remove:
#![allow(dead_code, unused_variables)]
use steel::steel_vm::engine::Engine;

pub(crate) fn register_types(steel_engine: &mut Engine) {
    common::register_types(steel_engine);
    directives::register_types(steel_engine);
    element::register_types(steel_engine);
    steel_date::register_types(steel_engine);
    steel_decimal::register_types(steel_engine);
}

pub(crate) mod common;
pub(crate) use common::*;
pub(crate) mod core;
pub(crate) use core::*;
pub(crate) mod directives;
pub(crate) use directives::*;
pub(crate) mod element;
pub(crate) use element::*;
pub(crate) mod steel_date;
pub(crate) use steel_date::*;
pub(crate) mod steel_decimal;
pub(crate) use steel_decimal::*;
