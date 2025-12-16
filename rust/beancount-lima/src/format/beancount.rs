use beancount_parser_lima as parser;
use std::fmt::{self, Display, Formatter};
use time::Date;

use super::*;
use crate::format::*;
use crate::loader::*;

// adapted from beancount-parser-lima

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use crate::loader::DirectiveVariant as LDV;
        use parser::DirectiveVariant as PDV;

        let directive = self.parsed.item();
        let date = *directive.date().item();

        match (self.parsed.variant(), &self.loaded) {
            (PDV::Transaction(parsed), LDV::Transaction(loaded)) => {
                loaded.fmt(f, date, parsed /*, &self.metadata*/)
            }
            (PDV::Pad(parsed), LDV::Pad(loaded)) => {
                loaded.fmt(f, date, directive /*, &self.metadata*/)
            }
            _ => writeln!(f, "{}", directive),
        }
    }
}

impl<'a> Transaction<'a> {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
        date: Date,
        parsed: &parser::Transaction, /*, metadata: &Metadata*/
    ) -> fmt::Result {
        write!(f, "{} {}", date, parsed.flag())?;

        format(f, parsed.payee(), double_quoted, SPACE, Some(SPACE))?;
        format(f, parsed.narration(), double_quoted, SPACE, Some(SPACE))?;
        // we prefer to show tags and links inline rather then line by line in metadata
        // metadata.fmt_tags_links_inline(f)?;
        // metadata.fmt_keys_values(f)?;
        format(
            f,
            self.postings.iter(),
            plain,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
        f.write_str(NEWLINE)
    }
}

impl<'a> Pad<'a> {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
        date: Date,
        parsed: &parser::Directive, /*, metadata: &Metadata*/
    ) -> fmt::Result {
        writeln!(f, "{}\n", parsed)?;
        write!(f, "{} {}", date, pad_flag())?;
        format(
            f,
            self.postings.iter(),
            plain,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
        f.write_str(NEWLINE)
    }
}

impl<'a> Display for Posting<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        simple_format(f, &self.flag, None)?;

        write!(
            f,
            "{}{} {} {}",
            if self.flag.is_some() { SPACE } else { EMPTY },
            &self.account,
            &self.units,
            &self.currency
        )?;

        simple_format(f, &self.cost, Some(SPACE))?;
        simple_format(f, &self.price, Some(SPACE))?;
        // self.metadata.fmt(f)

        Ok(())
    }
}
