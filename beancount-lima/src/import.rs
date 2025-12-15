use color_eyre::eyre::{eyre, Result};
pub(crate) use context::Context;
use std::{
    io::Write,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub(crate) struct Import {
    pub(crate) sources: Vec<Source>,
    pub(crate) context: Option<Context>,
}

#[derive(Debug)]
pub(crate) struct Source {
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
            .map(|path| Source::parse_from(path, error_w))
            .collect::<Result<Vec<_>>>()?;

        Ok(Import { sources, context })
    }
}

impl Source {
    pub(crate) fn parse_from<W>(path: &Path, _error_w: W) -> Result<Self>
    where
        W: Write + Copy,
    {
        match get_format(path)? {
            Format::Csv => csv::import(path),
            Format::Ofx => ofx::import(path),
        }
    }
}
mod context;
mod csv;
mod ofx;
