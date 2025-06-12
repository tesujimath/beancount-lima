pub(crate) use context::Context;
use std::{
    collections::HashMap,
    io::Write,
    path::{Path, PathBuf},
};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_derive::Steel;

use crate::{register_types_with_engine, AlistItem, Error};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Group {
    pub(crate) sources: Vec<Source>,
    pub(crate) context: Context,
}

#[derive(Clone, Debug, Steel)]
pub(crate) struct Source {
    pub(crate) header: Vec<AlistItem>,
    pub(crate) fields: Vec<String>,
    pub(crate) transactions: Vec<Vec<String>>,
}

enum Format {
    Csv,
    Ofx,
}

fn get_format(path: &Path) -> Result<Format, Error> {
    path.extension()
        .ok_or(Error::Cli(format!(
            "missing import file extension for {:?}",
            path
        )))
        .and_then(|ext| {
            if ext == "csv" || ext == "CSV" {
                Ok(Format::Csv)
            } else if ext == "ofx" || ext == "OFX" {
                Ok(Format::Ofx)
            } else {
                Err(Error::Cli(format!(
                    "unsupported import file extension {:?}",
                    ext
                )))
            }
        })
}

impl Group {
    pub(crate) fn parse_from<W>(
        paths: &[PathBuf],
        context: Context,
        error_w: W,
    ) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        let sources = paths
            .iter()
            .map(|path| Source::parse_from(path, error_w))
            .collect::<Result<Vec<Source>, Error>>()?;
        Ok(Group { sources, context })
    }

    fn sources(&self) -> Vec<Source> {
        self.sources.to_vec()
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
        steel_engine.register_fn("ffi-import-group-sources", Self::sources);
        steel_engine.register_fn("ffi-import-group-txnids", Self::txnids);
        steel_engine.register_fn("ffi-import-group-payees", Self::payees);
        steel_engine.register_fn("ffi-import-group-narrations", Self::narrations);
    }

    // TODO Ugh sort this and above
    pub(crate) fn register(self, steel_engine: &mut Engine) {
        register_types_with_engine(steel_engine);

        steel_engine
            .register_external_value("*ffi-import-group*", self)
            .unwrap(); // can't fail
    }
}

impl Source {
    pub(crate) fn parse_from<W>(path: &Path, _error_w: W) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        match get_format(path)? {
            Format::Csv => csv::import(path),
            Format::Ofx => ofx::import(path),
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

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Self>("ffi-import-source?");
        steel_engine.register_fn("ffi-import-source-header", Self::header);
        steel_engine.register_fn("ffi-import-source-fields", Self::fields);
        steel_engine.register_fn("ffi-import-source-transactions", Self::transactions);
    }
}

mod context;
mod csv;
mod ofx;
