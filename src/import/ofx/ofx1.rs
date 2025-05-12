use serde::Deserialize;

use super::Imported;
use crate::Error;

#[derive(Deserialize, Debug)]
struct Document {
    bankmsgsrsv1: BankMsgsRsV1,
}

#[derive(Deserialize, Debug)]
struct BankMsgsRsV1 {
    stmttrnrs: StmtTrnRs,
}

#[derive(Deserialize, Debug)]
struct StmtTrnRs {
    stmtrs: StmtRs,
}

#[derive(Deserialize, Debug)]
struct StmtRs {
    curdef: String,
    bankacctfrom: BankAcctFrom,
    banktranlist: BankTranList,
}

#[derive(Deserialize, Debug)]
struct BankAcctFrom {
    acctid: String,
}

#[derive(Deserialize, Debug)]
struct BankTranList {
    #[serde(rename = "stmttrn")]
    stmttrns: Vec<StmtTrn>,
}

#[derive(Deserialize, Debug)]
struct StmtTrn {
    trntype: String,
    dtposted: String,
    trnamt: String,
    fitid: String,
    name: String,
    memo: String,
}

impl StmtTrn {
    fn fields() -> Vec<String> {
        // TODO use a macro for pulling out all field names from the struct
        ["trntype", "dtposted", "trnamt", "fitid", "name", "memo"]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
    }

    fn values(self) -> Vec<String> {
        vec![
            self.trntype,
            self.dtposted,
            self.trnamt,
            self.fitid,
            self.name,
            self.memo,
        ]
    }
}

pub(crate) fn parse(ofx_content: &str) -> Result<Imported, Error> {
    let sgml = sgmlish::Parser::builder()
        .lowercase_names()
        .expand_entities(|entity| match entity {
            "lt" => Some("<"),
            "gt" => Some(">"),
            "amp" => Some("&"),
            "nbsp" => Some(" "),
            _ => None,
        })
        .parse(ofx_content)
        .map_err(Into::<Error>::into)?;
    let sgml = sgmlish::transforms::normalize_end_tags(sgml).map_err(Into::<Error>::into)?;
    let doc = sgmlish::from_fragment::<Document>(sgml).map_err(Into::<Error>::into)?;

    let Document {
        bankmsgsrsv1:
            BankMsgsRsV1 {
                stmttrnrs:
                    StmtTrnRs {
                        stmtrs:
                            StmtRs {
                                curdef,
                                bankacctfrom: BankAcctFrom { acctid },
                                banktranlist: BankTranList { stmttrns },
                            },
                    },
            },
    } = doc;

    Ok(Imported {
        header: vec![
            "format".to_string(),
            "ofx1".to_string(),
            "curdef".to_string(),
            curdef,
            "acctid".to_string(),
            acctid,
        ],
        fields: StmtTrn::fields(),
        transactions: stmttrns
            .into_iter()
            .map(StmtTrn::values)
            .collect::<Vec<_>>(),
    })
}

impl From<sgmlish::Error> for Error {
    fn from(value: sgmlish::Error) -> Self {
        Error::ImportFormat(format!("OFX format error: {}", value))
    }
}

impl From<sgmlish::de::DeserializationError> for Error {
    fn from(value: sgmlish::de::DeserializationError) -> Self {
        Error::ImportFormat(format!("OFX format error: {}", value))
    }
}

impl From<sgmlish::transforms::NormalizationError> for Error {
    fn from(value: sgmlish::transforms::NormalizationError) -> Self {
        Error::ImportFormat(format!("OFX format error: {}", value))
    }
}
