use slugify::slugify;
use std::{fs::File, path::Path};

use super::Imported;
use crate::Error;

pub(crate) fn import(path: &Path) -> Result<Imported, Error> {
    Err(Error::NotYetImplemented("OFX import"))
}
