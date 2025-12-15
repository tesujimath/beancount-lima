use rust_decimal::Decimal;
use time::Date;

use crate::loader::*;
use beancount_parser_lima::{self as parser, ElementType};
use std::fmt::{self, Display, Formatter, Write};
use std::iter::{once, repeat};
use strum_macros::{EnumIter, EnumString, IntoStaticStr};

// TODO improve this, it's a bit ugly
pub(crate) struct Edn<T>(pub(crate) T)
where
    T: FmtEdn + Clone;

impl<T> Display for Edn<T>
where
    T: FmtEdn + Clone,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        (self.0.clone()).fmt_edn(f)
    }
}

pub(crate) trait FmtEdn {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result;
}

impl<'a> FmtEdn for &Directive<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use crate::loader::DirectiveVariant as LDV;
        use parser::DirectiveVariant as PDV;

        let directive = self.parsed.item();
        let date = *directive.date().item();

        match (self.parsed.variant(), &self.loaded) {
            (PDV::Transaction(parsed), LDV::Transaction(loaded)) => {
                (loaded, date, parsed).fmt_edn(f /*, &self.metadata*/)
            }
            // (PDV::Pad(parsed), LDV::Pad(loaded)) => {
            //     loaded.fmt_edn(f, date, directive /*, &self.metadata*/)
            // }
            _ => {
                map_begin(f)?;
                (
                    Keyword::Directive,
                    directive.element_type(),
                    Separator::Flush,
                )
                    .fmt_edn(f)?;
                (Keyword::Narration, "not yet implemented", Separator::Spaced).fmt_edn(f)?;
                map_end(f)
            }
        }
    }
}

impl<'a> FmtEdn for (&Transaction<'a>, Date, &parser::Transaction<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (loaded, date, parsed) = self;
        map_begin(f)?;
        (Keyword::Directive, Keyword::Txn, Flush).fmt_edn(f)?;
        (Keyword::Date, date, Spaced).fmt_edn(f)?;
        (Keyword::Flag, *parsed.flag().item(), Spaced).fmt_edn(f)?;
        if let Some(payee) = parsed.payee().map(|x| *x.item()) {
            (Keyword::Payee, payee, Spaced).fmt_edn(f)?;
        }
        if let Some(narration) = parsed.narration().map(|x| *x.item()) {
            (Keyword::Narration, narration, Spaced).fmt_edn(f)?;
        }
        (Keyword::Postings, EdnVector(loaded.postings.iter()), Spaced).fmt_edn(f)?;

        map_end(f)
    }
}

impl<'a> FmtEdn for &Posting<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (Keyword::Account, self.account, Flush).fmt_edn(f)?;
        (Keyword::Units, self.units, Spaced).fmt_edn(f)?;
        (Keyword::Currency, self.currency, Spaced).fmt_edn(f)?;
        if let Some(cost) = self.cost.as_ref() {
            (Keyword::Cost, cost, Spaced).fmt_edn(f)?;
        }
        if let Some(price) = self.price.as_ref() {
            (Keyword::Price, price, Spaced).fmt_edn(f)?;
        }
        if let Some(flag) = self.flag {
            (Keyword::Flag, flag, Spaced).fmt_edn(f)?;
        }
        // TODO metadata

        map_end(f)
    }
}

impl<'a> FmtEdn for &Cost<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (Keyword::Date, self.date, Flush).fmt_edn(f)?;
        (Keyword::PerUnit, self.per_unit, Spaced).fmt_edn(f)?;
        (Keyword::Currency, self.currency, Spaced).fmt_edn(f)?;
        if let Some(label) = self.label {
            (Keyword::Label, label, Spaced).fmt_edn(f)?;
        }
        (Keyword::Merge, self.merge, Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for &Price<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (Keyword::PerUnit, self.per_unit, Flush).fmt_edn(f)?;
        (Keyword::Currency, self.currency, Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for parser::Currency<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt_edn(f)
    }
}

impl FmtEdn for Date {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, r#"#time/date "{}""#, self)
    }
}

impl FmtEdn for Decimal {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}M", self)
    }
}

impl FmtEdn for bool {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(if self { "true" } else { "false" })
    }
}

impl FmtEdn for &str {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        for c in self.chars() {
            match c {
                '\t' => f.write_str(r#"\t"#)?,
                '\r' => f.write_str(r#"\r"#)?,
                '\n' => f.write_str(r#"\\n"#)?,
                '\\' => f.write_str(r#"\\"#)?,
                '\"' => f.write_str(r#"\"\""#)?,
                c => f.write_char(c)?,
            }
        }
        f.write_char('"')
    }
}

// TODO: remove
// fn escape_string<'a>(s: &'a str) -> Cow<'a, str> {
//     fn requires_escape(c: &char) -> bool {
//         matches!(c, '\t' | '\r' | '\n' | '\\' | '"')
//     }

//     let n_escapes = s.chars().filter(requires_escape).count();
//     if n_escapes > 0 {
//         let mut escaped = String::with_capacity(s.len() + n_escapes);
//         for c in s.chars() {
//             match c {
//                 '\t' => escaped.push_str(r#"\t"#),
//                 '\r' => escaped.push_str(r#"\r"#),
//                 '\n' => escaped.push_str(r#"\\n"#),
//                 '\\' => escaped.push_str(r#"\\"#),
//                 '\"' => escaped.push_str(r#"\"\""#),
//                 c => escaped.push(c),
//             }
//         }
//         return Cow::Owned(escaped);
//     } else {
//         return Cow::Borrowed(s);
//     }
// }
//

impl FmtEdn for parser::Flag {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        self.to_string().fmt_edn(f)
    }
}

#[derive(EnumString, EnumIter, IntoStaticStr, Clone, Debug)]
#[strum(serialize_all = "kebab-case")]
enum Keyword {
    Account,
    Cost,
    Currency,
    Date,
    Directive,
    Flag,
    Label,
    Merge,
    Narration,
    Payee,
    PerUnit,
    Postings,
    Price,
    Txn,
    Units,
}

impl FmtEdn for Keyword {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, ":{}", Into::<&str>::into(self))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Separator {
    Flush,
    Spaced,
}

struct EdnVector<I, T>(I)
where
    I: Iterator<Item = T>;

impl<I, T> FmtEdn for EdnVector<I, T>
where
    I: Iterator<Item = T>,
    T: FmtEdn,
{
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;

        for (item, sep) in self.0.zip(separators()) {
            if sep == Separator::Spaced {
                f.write_str(" ")?;
            }
            item.fmt_edn(f)?;
        }
        f.write_str("]")
    }
}

// a map entry with optional separator prefix
//
// we can't implement map formatting like we did with sets and vectors because of the heterogeny
impl<T> FmtEdn for (Keyword, T, Separator)
where
    T: FmtEdn,
{
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        let (name, value, sep) = self;
        if sep == Separator::Spaced {
            f.write_str(", ")?;
        }
        name.fmt_edn(f)?;
        f.write_str(" ")?;
        value.fmt_edn(f)
    }
}

fn map_begin(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("{")
}

fn map_end(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str("}")
}

// an infinite iterator of separators, with the first one only being flush
fn separators() -> impl Iterator<Item = Separator> {
    use Separator::*;

    once(Flush).chain(repeat(Spaced))
}

/// TODO this is ugh
pub(crate) const VECTOR_BEGIN: &str = "[";
pub(crate) const VECTOR_END: &str = "]";
