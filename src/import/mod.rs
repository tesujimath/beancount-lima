use color_eyre::eyre::{eyre, Result};
pub(crate) use context::Context;
use std::{
    io::Write,
    path::{Path, PathBuf},
};
use steel::{
    gc::Gc,
    rvals::{IntoSteelVal, SteelHashMap, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal,
};
use steel_derive::Steel;

#[derive(Clone, Debug, Steel)]
pub(crate) struct Import {
    pub(crate) sources: SteelVector, // VectorV<SteelVal>,
    pub(crate) context: Option<SteelVal>,
}

#[derive(Clone, Debug, Steel)]
pub(crate) struct Source {
    pub(crate) header: SteelHashMap, // HashMap<SteelVal::SymbolV, SteelVal>
    pub(crate) fields: Vec<SteelVal>,
    pub(crate) transactions: Vec<SteelVal>,
}

#[derive(Debug)]
pub(crate) struct RawSource {
    pub(crate) header: Vec<(&'static str, String)>,
    pub(crate) fields: Vec<String>,
    pub(crate) transactions: Vec<Vec<String>>,
}

enum Format {
    Csv,
    Ofx,
}

fn get_format(path: &Path) -> Result<Format> {
    path.extension()
        .ok_or(eyre!("missing import file extension for {:?}", path))
        .and_then(|ext| {
            if ext == "csv" || ext == "CSV" {
                Ok(Format::Csv)
            } else if ext == "ofx" || ext == "OFX" {
                Ok(Format::Ofx)
            } else {
                Err(eyre!("unsupported import file extension {:?}", ext))
            }
        })
}

impl Import {
    pub(crate) fn parse_from<W>(
        paths: &[PathBuf],
        context: Option<Context>,
        error_w: W,
    ) -> Result<Self>
    where
        W: Write + Copy,
    {
        let sources = paths
            .iter()
            .map(|path| {
                Source::parse_from(path, error_w).map(|source| source.into_steelval().unwrap())
            })
            .collect::<Result<steel::Vector<SteelVal>>>()?;
        let sources = Gc::new(sources).into();

        let context = context.map(|context| context.into_steelval().unwrap());

        Ok(Import { sources, context })
    }

    fn sources(&self) -> SteelVal {
        SteelVal::VectorV(self.sources.clone())
    }

    fn context(&self) -> Option<SteelVal> {
        self.context.clone()
    }

    pub(crate) fn register(self, steel_engine: &mut Engine) {
        steel_engine
            .register_external_value("*import*", self)
            .unwrap(); // can't fail
    }
}

impl Source {
    pub(crate) fn parse_from<W>(path: &Path, _error_w: W) -> Result<Self>
    where
        W: Write + Copy,
    {
        let RawSource {
            header,
            fields,
            transactions,
        } = match get_format(path)? {
            Format::Csv => csv::import(path)?,
            Format::Ofx => ofx::import(path)?,
        };

        let header = header
            .into_iter()
            .map(|(k, v)| {
                (
                    SteelVal::SymbolV(k.to_string().into()),
                    v.into_steelval().unwrap(),
                )
            })
            .collect::<steel::HashMap<_, _>>();
        let header = Gc::new(header).into();

        let fields = fields
            .into_iter()
            .map(|s| s.into_steelval().unwrap())
            .collect::<Vec<_>>();

        let transactions = transactions
            .into_iter()
            .map(|txn| {
                txn.into_iter()
                    .map(|s| s.into_steelval().unwrap())
                    .collect::<Vec<_>>()
                    .into_steelval()
                    .unwrap()
            })
            .collect::<Vec<_>>();

        Ok(Self {
            header,
            fields,
            transactions,
        })
    }

    fn header(&self) -> SteelVal {
        SteelVal::HashMapV(self.header.clone())
    }

    fn fields(&self) -> Vec<SteelVal> {
        self.fields.clone()
    }

    fn transactions(&self) -> Vec<SteelVal> {
        self.transactions.clone()
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<Import>("import?");
    steel_engine.register_fn("import-sources", Import::sources);
    steel_engine.register_fn("import-context", Import::context);

    steel_engine.register_type::<Source>("import-source?");
    steel_engine.register_fn("import-source-header", Source::header);
    steel_engine.register_fn("import-source-fields", Source::fields);
    steel_engine.register_fn("import-source-transactions", Source::transactions);

    context::register_types(steel_engine);
}

mod context;
mod csv;
mod ofx;
