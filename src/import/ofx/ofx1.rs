use std::path::Path;

use serde::Deserialize;

use crate::{import::Source, Error};

#[derive(Deserialize, Debug)]
struct Document {
    bankmsgsrsv1: Option<BankMsgsRsV1>,
    creditcardmsgsrsv1: Option<CreditCardMsgsRsV1>,
}

#[derive(Deserialize, Debug)]
struct BankMsgsRsV1 {
    stmttrnrs: StmtTrnRs,
}

#[derive(Deserialize, Debug)]
struct CreditCardMsgsRsV1 {
    ccstmttrnrs: CcStmtTrnRs,
}

#[derive(Deserialize, Debug)]
struct StmtTrnRs {
    stmtrs: StmtRs,
}

#[derive(Deserialize, Debug)]
struct CcStmtTrnRs {
    ccstmtrs: CcStmtRs,
}

#[derive(Deserialize, Debug)]
struct StmtRs {
    curdef: String,
    bankacctfrom: BankAcctFrom,
    banktranlist: BankTranList,
    ledgerbal: LedgerBal,
}

#[derive(Deserialize, Debug)]
struct CcStmtRs {
    curdef: String,
    ccacctfrom: CcAcctFrom,
    banktranlist: BankTranList,
    ledgerbal: LedgerBal,
}

#[derive(Deserialize, Debug)]
struct BankAcctFrom {
    acctid: String,
}

#[derive(Deserialize, Debug)]
struct CcAcctFrom {
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

#[derive(Deserialize, Debug)]
struct LedgerBal {
    balamt: String,
    dtasof: String,
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

pub(crate) fn parse(path: &Path, ofx_content: &str) -> Result<Source, Error> {
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

    match doc {
        Document {
            bankmsgsrsv1:
                Some(BankMsgsRsV1 {
                    stmttrnrs:
                        StmtTrnRs {
                            stmtrs:
                                StmtRs {
                                    curdef,
                                    bankacctfrom: BankAcctFrom { acctid },
                                    banktranlist: BankTranList { stmttrns },
                                    ledgerbal: LedgerBal { balamt, dtasof },
                                },
                        },
                }),
            creditcardmsgsrsv1: None,
        } => Ok((curdef, acctid, balamt, dtasof, stmttrns)),

        Document {
            bankmsgsrsv1: None,
            creditcardmsgsrsv1:
                Some(CreditCardMsgsRsV1 {
                    ccstmttrnrs:
                        CcStmtTrnRs {
                            ccstmtrs:
                                CcStmtRs {
                                    curdef,
                                    ccacctfrom: CcAcctFrom { acctid },
                                    banktranlist: BankTranList { stmttrns },
                                    ledgerbal: LedgerBal { balamt, dtasof },
                                },
                        },
                }),
        } => Ok((curdef, acctid, balamt, dtasof, stmttrns)),

        _ => Err(Error::ImportFormat("unsupported OFX1 document".to_string())),
    }
    .map(|(curdef, acctid, balamt, dtasof, stmttrns)| Source {
        header: vec![
            ("format", "ofx1").into(),
            ("curdef", curdef).into(),
            ("acctid", acctid).into(),
            ("balamt", balamt).into(),
            ("dtasof", dtasof).into(),
            ("path", path.to_string_lossy()).into(),
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
