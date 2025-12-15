use color_eyre::eyre::{eyre, Result};
use regex::Regex;
use std::{fs::read_to_string, path::Path};

use crate::import::Source;

pub(crate) fn import(path: &Path) -> Result<Source> {
    let ofx_content = read_to_string(path)?;
    let first_line = ofx_content.lines().next();
    if let Some(first_line) = first_line {
        if first_line.trim() == "OFXHEADER:100" {
            let blank_line = Regex::new("\r\n\\s*\r\n").unwrap();
            if let Some(m) = blank_line.find(&ofx_content) {
                ofx1::parse(path, &ofx_content[m.end()..])
            } else {
                Err(eyre!("failed to find end of OFX1 header in {:?}", path))
            }
        } else {
            Err(eyre!("OFX2 not supported"))
        }
    } else {
        Err(eyre!("failed to read first line in {:?}", path))
    }
}

mod ofx1;
