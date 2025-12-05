use beancount_parser_lima as parser;
use std::fmt::{self, Display, Formatter};
use time::Date;

use super::*;
use crate::format::*;

// adapted from beancount-parser-lima

impl Display for Directive {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use directives::DirectiveVariant::*;

        match &self.variant {
            Transaction(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Price(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Balance(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Open(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Close(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Commodity(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Pad(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Document(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Note(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Event(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Query(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Custom(x) => x.fmt(f, self.date /*, &self.metadata*/),
        }
    }
}

impl directives::Transaction {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} {}", date, &self.flag)?;

        format(f, &self.payee, double_quoted, SPACE, Some(SPACE))?;
        format(f, &self.narration, double_quoted, SPACE, Some(SPACE))?;
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

impl directives::Price {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} price {} {}", date, &self.currency, &self.amount)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Balance {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} balance {} {}", date, &self.account, &self.amount)?;
        simple_format(f, self.tolerance.as_ref(), Some(TILDE_SPACE))?;

        // we prefer to show tags and links inline rather then line by line in metadata
        // metadata.fmt_tags_links_inline(f)?;
        // metadata.fmt_keys_values(f)?;

        f.write_str(NEWLINE)
    }
}

impl directives::Open {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} open {}", date, &self.account)?;
        format(f, &self.currencies, plain, COMMA, Some(SPACE))?;
        format(
            f,
            self.booking.map(Into::<parser::Booking>::into),
            double_quoted,
            EMPTY, // irrelevant as at most one value
            Some(SPACE),
        )?;
        f.write_str(NEWLINE)
    }
}

impl directives::Close {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} close {}", date, &self.account)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Commodity {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} commodity {}", date, &self.currency)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Pad {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} pad {} {}", date, &self.account, &self.source)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Document {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} document {} \"{}\"", date, &self.account, &self.path)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Note {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} note {} \"{}\"", date, &self.account, &self.comment)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Event {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(
            f,
            "{} event \"{}\" \"{}\"",
            date, &self.event_type, &self.description
        )?;
        f.write_str(NEWLINE)
    }
}

impl directives::Query {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} query \"{}\" \"{}\"", date, &self.name, &self.content)?;
        f.write_str(NEWLINE)
    }
}

impl directives::Custom {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(
            f,
            "{} custom \"{}\" ; TODO values not yet supported",
            date, &self.type_
        )?;
        f.write_str(NEWLINE)
    }
}

impl Display for Posting {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        simple_format(f, &self.flag, None)?;

        write!(
            f,
            "{}{} {}",
            if self.flag.is_some() { SPACE } else { EMPTY },
            &self.account,
            &self.amount
        )?;

        simple_format(f, &self.cost, Some(SPACE))?;
        simple_format(f, &self.price, Some(SPACE))?;
        // self.metadata.fmt(f)

        Ok(())
    }
}
