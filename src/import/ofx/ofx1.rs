use color_eyre::eyre::{eyre, Result};
use serde::Deserialize;
use std::path::Path;

use crate::import::Source;

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

pub(crate) fn parse(path: &Path, ofx_content: &str) -> Result<Source> {
    let sgml = sgmlish::Parser::builder()
        .lowercase_names()
        .expand_entities(|entity| match entity {
            "lt" => Some("<"),
            "gt" => Some(">"),
            "amp" => Some("&"),
            "nbsp" => Some(" "),
            _ => None,
        })
        .parse(ofx_content)?;
    let sgml = sgmlish::transforms::normalize_end_tags(sgml)?;
    let doc = sgmlish::from_fragment::<Document>(sgml)?;

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

        _ => Err(eyre!("unsupported OFX1 document {:?}", path)),
    }
    .map(|(curdef, acctid, balamt, dtasof, stmttrns)| Source {
        header: vec![
            ("format", "ofx1".to_string()).into(),
            ("curdef", curdef).into(),
            ("acctid", acctid).into(),
            ("balamt", balamt).into(),
            ("dtasof", dtasof).into(),
            ("path", path.to_string_lossy().into_owned()).into(),
        ],
        fields: StmtTrn::fields(),
        transactions: stmttrns
            .into_iter()
            .map(StmtTrn::values)
            .collect::<Vec<_>>(),
    })
}
