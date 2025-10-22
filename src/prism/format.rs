use beancount_parser_lima as parser;
use lazy_format::lazy_format;
use std::fmt::{self, Display, Formatter};
use time::Date;

use super::*;

// adapted from beancount-parser-lima

impl Display for Directive {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use DirectiveVariant::*;

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
        }
    }
}

impl Transaction {
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

impl Price {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} price {} {}", date, &self.currency, &self.amount)?;
        f.write_str(NEWLINE)
    }
}

impl Balance {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} balance {} {}", date, &self.account, &self.amount)?;
        simple_format(f, self.tolerance.as_ref(), Some(TILDE_SPACE))?;

        // we prefer to show tags and links inline rather then line by line in metadata
        // metadata.fmt_tags_links_inline(f)?;
        // metadata.fmt_keys_values(f)?;

        f.write_str(NEWLINE)
    }
}

impl Open {
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

impl Close {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} close {}", date, &self.account)?;
        f.write_str(NEWLINE)
    }
}

impl Commodity {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} commodity {}", date, &self.currency)?;
        f.write_str(NEWLINE)
    }
}

impl Pad {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} pad {} {}", date, &self.account, &self.source)?;
        f.write_str(NEWLINE)
    }
}

impl Document {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} document {} \"{}\"", date, &self.account, &self.path)?;
        f.write_str(NEWLINE)
    }
}

impl Note {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} note {} \"{}\"", date, &self.account, &self.comment)?;
        f.write_str(NEWLINE)
    }
}

impl Event {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(
            f,
            "{} event \"{}\" \"{}\"",
            date, &self.event_type, &self.description
        )?;
        f.write_str(NEWLINE)
    }
}

impl Query {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
        write!(f, "{} query \"{}\" \"{}\"", date, &self.name, &self.content)?;
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
        // simple_format(f, &self.price_annotation, Some(" @ "))?;
        // self.metadata.fmt(f)

        Ok(())
    }
}

pub(crate) fn format<C, T, M, D>(
    f: &mut Formatter<'_>,
    container: C,
    mapper: M,
    separator: &'static str,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    M: Fn(T) -> D,
    D: Display,
{
    let mut container = container.into_iter();
    if let Some(item) = container.by_ref().next() {
        if let Some(prefix) = prefix {
            f.write_str(prefix)?;
        }

        mapper(item).fmt(f)?;
    }

    for item in container {
        f.write_str(separator)?;
        mapper(item).fmt(f)?;
    }

    Ok(())
}

/// Simple format with no mapper or separator
pub(crate) fn simple_format<C, T>(
    f: &mut Formatter<'_>,
    container: C,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    T: Display,
{
    format(f, container, plain, EMPTY, prefix)
}

/// Format plain.
pub(crate) fn plain<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("{s}")
}

/// Format in double quotes.
pub(crate) fn double_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("\"{s}\"")
}

/// Format key/value.
pub(crate) fn key_value<K, V>(kv: (K, V)) -> impl Display
where
    K: Display,
    V: Display,
{
    lazy_format!("{}: {}", kv.0, kv.1)
}

pub(crate) const EMPTY: &str = "";
pub(crate) const SPACE: &str = " ";
pub(crate) const COMMA: &str = ",";
pub(crate) const TILDE_SPACE: &str = " ~ ";
pub(crate) const NEWLINE: &str = "\n";
pub(crate) const NEWLINE_INDENT: &str = "\n  ";
