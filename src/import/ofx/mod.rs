use regex::Regex;
use std::{fs::read_to_string, path::Path};

use crate::{import::Source, Error};

pub(crate) fn import(path: &Path) -> Result<Source, Error> {
    let ofx_content = read_to_string(path).map_err(Into::<Error>::into)?;
    let first_line = ofx_content.lines().next();
    if let Some(first_line) = first_line {
        if first_line.trim() == "OFXHEADER:100" {
            let blank_line = Regex::new("\r\n\\s*\r\n").unwrap();
            if let Some(m) = blank_line.find(&ofx_content) {
                ofx1::parse(&ofx_content[m.end()..])
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
