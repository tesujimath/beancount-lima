use color_eyre::eyre::Result;
use slugify::slugify;
use std::path::Path;

use super::RawSource;

pub(crate) fn import(path: &Path) -> Result<RawSource> {
    let csv_file = std::fs::File::open(path)?;
    let mut rdr = csv::Reader::from_reader(csv_file);
    let fields = rdr
        .headers()?
        .iter()
        .map(|field| slugify(field, "", "-", None))
        .collect::<Vec<_>>();
    let mut transactions = Vec::<Vec<String>>::default();
    for transaction in rdr.records() {
        // The iterator yields Result<StringRecord, Error>, so we check the
        // error here..
        let transaction = transaction?
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        transactions.push(transaction);
    }

    let header = vec![
        ("format", "csv".to_string()),
        ("path", path.to_string_lossy().into_owned()),
    ];

    Ok(RawSource {
        header,
        fields,
        transactions,
    })
}
