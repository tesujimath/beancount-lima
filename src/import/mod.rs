use context::ImportContext;
use std::{io::Write, path::Path};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_derive::Steel;

use crate::{register_types_with_engine, Error};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Imported {
    pub(crate) context: ImportContext,
    pub(crate) header: Vec<String>,
    pub(crate) fields: Vec<String>,
    pub(crate) transactions: Vec<Vec<String>>,
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

    fn header(&self) -> Vec<String> {
        self.header.clone()
    }

    fn fields(&self) -> Vec<String> {
        self.fields.clone()
    }

    fn transactions(&self) -> Vec<Vec<String>> {
        self.transactions.clone()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Self>("ffi-imported?");
        steel_engine.register_fn("ffi-imported-header", Self::header);
        steel_engine.register_fn("ffi-imported-fields", Self::fields);
        steel_engine.register_fn("ffi-imported-transactions", Self::transactions);
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
