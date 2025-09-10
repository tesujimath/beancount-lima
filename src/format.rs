use lazy_format::lazy_format;
use std::fmt::{self, Display, Formatter};

use crate::{steel_date::SteelDate, types::*};

// adapted from beancount-parser-lima

impl Display for Directive {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use DirectiveVariant::*;

        match &self.variant {
            Transaction(x) => x.fmt(f, self.date /*, &self.metadata*/),
            // TODO
            Price(x) => Ok(()), //x.fmt(f, self.date /*, &self.metadata*/),
            Balance(x) => x.fmt(f, self.date /*, &self.metadata*/),
            Open(x) => Ok(()),      //x.fmt(f, self.date /*, &self.metadata*/),
            Close(x) => Ok(()),     //x.fmt(f, self.date /*, &self.metadata*/),
            Commodity(x) => Ok(()), //x.fmt(f, self.date /*, &self.metadata*/),
            Pad(x) => Ok(()),       //x.fmt(f, self.date /*, &self.metadata*/),
            Document(x) => Ok(()),  //x.fmt(f, self.date /*, &self.metadata*/),
            Note(x) => Ok(()),      //x.fmt(f, self.date /*, &self.metadata*/),
            Event(x) => Ok(()),     //x.fmt(f, self.date /*, &self.metadata*/),
            Query(x) => Ok(()),     //x.fmt(f, self.date /*, &self.metadata*/),
        }
    }
}

impl Transaction {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
        date: SteelDate, /*, metadata: &Metadata*/
    ) -> fmt::Result {
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

impl Balance {
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
        date: SteelDate, /*, metadata: &Metadata*/
    ) -> fmt::Result {
        write!(f, "{} balance {} {}", date, &self.account, &self.amount)?;
        simple_format(f, self.tolerance.as_ref(), Some(TILDE_SPACE))?;

        // we prefer to show tags and links inline rather then line by line in metadata
        // metadata.fmt_tags_links_inline(f)?;
        // metadata.fmt_keys_values(f)?;

        f.write_str(NEWLINE)
    }
}

// impl Commodity {
//     fn fmt(&self, f: &mut Formatter<'_>, date: Date /*, metadata: &Metadata*/) -> fmt::Result {
//         write!(f, "{} commodity {}", date, self.currency)?;
//         // we prefer to show tags and links inline rather then line by line in metadata
//         // metadata.fmt_tags_links_inline(f)?;
//         // metadata.fmt_keys_values(f)
//     }
// }

impl Display for Posting {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        simple_format(f, &self.flag, None)?;

        write!(
            f,
            "{}{} {}",
            if self.flag.is_some() { SPACE } else { EMPTY },
            &self.account,
            &self.amount
        )

        // simple_format(f, &self.cost_spec, Some(SPACE))?;
        // simple_format(f, &self.price_annotation, Some(" @ "))?;

        // self.metadata.fmt(f)
    }
}

pub fn format<C, T, M, D>(
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
pub fn simple_format<C, T>(
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
pub fn plain<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("{s}")
}

/// Format in single quotes.
pub fn single_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("'{s}'")
}

/// Format in double quotes.
pub fn double_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("\"{s}\"")
}

/// Format key/value.
pub fn key_value<K, V>(kv: (K, V)) -> impl Display
where
    K: Display,
    V: Display,
{
    lazy_format!("{}: {}", kv.0, kv.1)
}

fn pad_if(condition: bool) -> &'static str {
    if condition {
        SPACE
    } else {
        EMPTY
    }
}

pub const EMPTY: &str = "";
pub const SPACE: &str = " ";
pub const TILDE_SPACE: &str = " ~ ";
pub const NEWLINE: &str = "\n";
pub const INDENT: &str = "  ";
pub const NEWLINE_INDENT: &str = "\n  ";
