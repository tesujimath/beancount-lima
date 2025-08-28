use lazy_format::lazy_format;
use std::fmt::Display;

use crate::types::*;

impl Display for Posting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(flag) = &self.flag {
            write!(f, "{} ", flag_to_string(flag))?
        }
        write!(f, "{} {}", &self.account, &self.amount)
    }
}

pub(crate) fn flag_to_string(flag: &str) -> impl Display {
    let flag = flag.chars().next().unwrap();

    lazy_format!(
        "{}{}",
        if flag.is_ascii_uppercase() { "\'" } else { "" },
        flag
    )
}
