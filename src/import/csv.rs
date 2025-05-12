use slugify::slugify;
use std::{fs::File, path::Path};

use super::Imported;
use crate::Error;

pub(crate) fn import(path: &Path) -> Result<Imported, Error> {
    let csv_file = File::open(path).map_err(Error::Io)?;
    let mut rdr = csv::Reader::from_reader(csv_file);
    let fields = rdr
        .headers()
        .map_err(Error::Csv)?
        .iter()
        .map(|field| slugify(field, "", "-", None))
        .collect::<Vec<_>>();
    let mut transactions = Vec::<Vec<String>>::default();
    for transaction in rdr.records() {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here..
        let transaction = transaction
            .map_err(Error::Csv)?
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        transactions.push(transaction);
    }

    Ok(Imported {
        header: Vec::default(),
        fields,
        transactions,
    })
}
