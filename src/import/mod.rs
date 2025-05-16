use context::ImportContext;
use std::{collections::HashMap, io::Write, path::Path};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_derive::Steel;

use crate::{register_types_with_engine, AlistItem, Error};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Imported {
    pub(crate) header: Vec<AlistItem>,
    pub(crate) fields: Vec<String>,
    pub(crate) transactions: Vec<Vec<String>>,
    pub(crate) context: ImportContext,
}

enum Format {
    Csv,
    Ofx,
}

fn get_format(path: &Path) -> Option<Format> {
    path.extension().and_then(|ext| {
        if ext == "csv" || ext == "CSV" {
            Some(Format::Csv)
        } else if ext == "ofx" || ext == "OFX" {
            Some(Format::Ofx)
        } else {
            None
        }
    })
}

impl Imported {
    pub(crate) fn parse_from<W>(
        path: &Path,
        context: ImportContext,
        error_w: W,
    ) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        match get_format(path) {
            Some(Format::Csv) => csv::import(path, context),
            Some(Format::Ofx) => ofx::import(path, context),
            None => Err(Error::Cli("unsupported import file extension".to_string())),
        }
    }

    fn header(&self) -> Vec<AlistItem> {
        self.header.clone()
    }

    fn fields(&self) -> Vec<String> {
        self.fields.clone()
    }

    fn transactions(&self) -> Vec<Vec<String>> {
        self.transactions.clone()
    }

    fn txnids(&self) -> Vec<String> {
        self.context.txnids()
    }

    fn payees(&self) -> HashMap<String, HashMap<String, isize>> {
        self.context.payees()
    }

    fn narrations(&self) -> HashMap<String, HashMap<String, isize>> {
        self.context.narrations()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Self>("ffi-imported?");
        steel_engine.register_fn("ffi-imported-header", Self::header);
        steel_engine.register_fn("ffi-imported-fields", Self::fields);
        steel_engine.register_fn("ffi-imported-transactions", Self::transactions);
        steel_engine.register_fn("ffi-imported-txnids", Self::txnids);
        steel_engine.register_fn("ffi-imported-payees", Self::payees);
        steel_engine.register_fn("ffi-imported-narrations", Self::narrations);
    }

    // TODO Ugh sort this and above
    pub(crate) fn register(self, steel_engine: &mut Engine) {
        register_types_with_engine(steel_engine);

        steel_engine
            .register_external_value("*ffi-imported*", self)
            .unwrap(); // can't fail
    }
}

pub(crate) mod context;
mod csv;
mod ofx;
