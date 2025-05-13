use std::{fs::read_to_string, path::Path};

use crate::Error;

use super::{ImportContext, Imported};

pub(crate) fn import(path: &Path, context: ImportContext) -> Result<Imported, Error> {
    let ofx_content = read_to_string(path).map_err(Into::<Error>::into)?;
    let first_line = ofx_content.lines().next();
    if let Some(first_line) = first_line {
        if first_line == "OFXHEADER:100" {
            let blank_line = "\r\n\r\n";
            if let Some(doc_start) = ofx_content.find(blank_line) {
                ofx1::parse(&ofx_content[doc_start..], context)
            } else {
                Err(Error::ImportFormat(
                    "failed to find end of OFX1 header".to_string(),
                ))
            }
        } else {
            Err(Error::NotYetImplemented("OFX2 not supported"))
        }
    } else {
        Err(Error::ImportFormat("failed to read first line".to_string()))
    }
}

mod ofx1;
