use slugify::slugify;
use std::{fs::File, io::Write, path::Path};
use steel::steel_vm::{engine::Engine, register_fn::RegisterFn};
use steel_derive::Steel;

use crate::{register_types_with_engine, Error};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Import {
    transaction_fields: Vec<String>,
    transactions: Vec<Vec<String>>,
}

impl Import {
    pub(crate) fn parse_from<W>(path: &Path, error_w: W) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        // TODO look at file extension and parse accordingly
        // For now we only do CSV
        let csv_file = File::open(path).map_err(Error::Io)?;
        let mut rdr = csv::Reader::from_reader(csv_file);
        let transaction_fields = rdr
            .headers()
            .map_err(Error::Csv)?
            .iter()
            .map(|field| slugify(field, "", "-", None))
            .collect::<Vec<_>>();
        let mut transactions = Vec::<Vec<String>>::default();
        for trasnaction in rdr.records() {
            // The iterator yields Result<StringRecord, Error>, so we check the
            // error here..
            let transaction = trasnaction
                .map_err(Error::Csv)?
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>();
            transactions.push(transaction);
        }

        Ok(Import {
            transaction_fields,
            transactions,
        })
    }

    fn transaction_fields(&self) -> Vec<String> {
        self.transaction_fields.clone()
    }

    fn transactions(&self) -> Vec<Vec<String>> {
        self.transactions.clone()
    }

    pub(crate) fn register_with_engine(steel_engine: &mut Engine) {
        steel_engine.register_type::<Self>("ffi-import?");
        steel_engine.register_fn("ffi-import-transaction-fields", Self::transaction_fields);
        steel_engine.register_fn("ffi-import-transactions", Self::transactions);
    }

    // TODO Ugh sort this and above
    pub(crate) fn register(self, steel_engine: &mut Engine) {
        register_types_with_engine(steel_engine);

        steel_engine
            .register_external_value("*ffi-import*", self)
            .unwrap(); // can't fail
    }
}
