use std::fmt::{self, Display, Formatter};

// this one is subtly different from the one in beancount-parser-lima
pub(crate) fn format<C, T, F>(
    f: &mut Formatter<'_>,
    container: C,
    item_fmt: F,
    separator: &'static str,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    F: Fn(&mut Formatter<'_>, T) -> fmt::Result,
{
    let mut container = container.into_iter();
    if let Some(item) = container.by_ref().next() {
        if let Some(prefix) = prefix {
            f.write_str(prefix)?;
        }

        item_fmt(f, item)?;
    }

    for item in container {
        f.write_str(separator)?;
        item_fmt(f, item)?;
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
pub(crate) fn plain<S>(f: &mut Formatter<'_>, s: S) -> fmt::Result
where
    S: Display,
{
    write!(f, "{s}")
}

/// Format in double quotes.
pub(crate) fn double_quoted<S>(f: &mut Formatter<'_>, s: S) -> fmt::Result
where
    S: Display,
{
    write!(f, "\"{s}\"")
}

// Format key/value.
// pub(crate) fn key_value<K, V>(f: &mut Formatter<'_>, kv: (K, V)) -> fmt::Result
// where
//     K: Display,
//     V: Display,
// {
//     write!(f, "{}: {}", kv.0, kv.1)
// }

pub(crate) const EMPTY: &str = "";
pub(crate) const SPACE: &str = " ";
pub(crate) const COMMA: &str = ",";
pub(crate) const TILDE_SPACE: &str = " ~ ";
pub(crate) const NEWLINE: &str = "\n";
pub(crate) const NEWLINE_INDENT: &str = "\n  ";

pub(crate) const FLUSH: &str = "";
pub(crate) const GUTTER_MINOR: &str = " ";
pub(crate) const GUTTER_MEDIUM: &str = "  ";
pub(crate) const GUTTER_MAJOR: &str = "   ";
