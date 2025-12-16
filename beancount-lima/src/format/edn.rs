use rust_decimal::Decimal;
use time::Date;

use crate::loader::*;
use beancount_parser_lima as parser;
use std::fmt::{self, Display, Formatter, Write};
use std::iter::{empty, once, repeat};
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
                (loaded, date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Pad(parsed), LDV::Pad(loaded)) => {
                (loaded, date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Price(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Balance(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Open(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Close(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Commodity(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Document(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Note(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Event(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Query(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (PDV::Custom(parsed), _) => {
                (date, parsed).fmt_edn(f) // TODO metadata
            }
            (parsed, loaded) => panic!("impossible combination of {parsed:?} and {loaded:?}"),
        }
    }
}

impl<'a> FmtEdn for (&Transaction<'a>, Date, &parser::Transaction<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (loaded, date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Txn, Spaced).fmt_edn(f)?;
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
impl<'a> FmtEdn for (&Pad<'a>, Date, &parser::Pad<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (loaded, date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Pad, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        (Keyword::Source, parsed.source().item().as_ref(), Spaced).fmt_edn(f)?;
        map_end(f)?;

        f.write_str(NEWLINE)?;

        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Txn, Spaced).fmt_edn(f)?;
        (Keyword::Flag, pad_flag(), Spaced).fmt_edn(f)?;
        (Keyword::Postings, EdnVector(loaded.postings.iter()), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Price<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        let price = Price {
            per_unit: parsed.amount().number().value(),
            currency: *parsed.amount().currency().item(),
        };
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Price, Spaced).fmt_edn(f)?;
        (Keyword::Currency, parsed.currency().item().as_ref(), Spaced).fmt_edn(f)?;
        (Keyword::Price, &price, Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Balance<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Balance, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        (
            Keyword::Units,
            parsed.atol().amount().number().value(),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::Currency,
            *parsed.atol().amount().currency().item(),
            Spaced,
        )
            .fmt_edn(f)?;
        if let Some(tolerance) = parsed.atol().tolerance() {
            (Keyword::Tolerance, *tolerance.item(), Spaced).fmt_edn(f)?;
        }
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Open<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Open, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        (
            Keyword::Currencies,
            EdnSet(parsed.currencies().map(|cur| *cur.item())),
            Spaced,
        )
            .fmt_edn(f)?;
        if let Some(booking) = parsed.booking() {
            (Keyword::Booking, *booking.item(), Spaced).fmt_edn(f)?;
        }
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Close<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Close, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Commodity<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Commodity, Spaced).fmt_edn(f)?;
        (Keyword::Currency, *parsed.currency().item(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Document<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Document, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        (Keyword::Path, *parsed.path().item(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Note<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Note, Spaced).fmt_edn(f)?;
        (Keyword::Account, parsed.account().item().as_ref(), Spaced).fmt_edn(f)?;
        (Keyword::Comment, *parsed.comment().item(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Event<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Event, Spaced).fmt_edn(f)?;
        (Keyword::Type, *parsed.event_type().item(), Spaced).fmt_edn(f)?;
        (Keyword::Description, *parsed.description().item(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Query<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Query, Spaced).fmt_edn(f)?;
        (Keyword::Name, *parsed.name().item(), Spaced).fmt_edn(f)?;
        (Keyword::Content, *parsed.content().item(), Spaced).fmt_edn(f)?;
        map_end(f)
    }
}

impl<'a> FmtEdn for (Date, &parser::Custom<'a>) {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        let (date, parsed) = self;
        map_begin(f)?;
        (Keyword::Date, date, Flush).fmt_edn(f)?;
        (Keyword::Directive, Keyword::Custom, Spaced).fmt_edn(f)?;
        (Keyword::Type, *parsed.type_().item(), Spaced).fmt_edn(f)?;
        // TODO custom values, with metadata
        (Keyword::Values, EdnVector(empty::<&str>()), Spaced).fmt_edn(f)?;
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

impl FmtEdn for parser::Booking {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use beancount_parser_lima::Booking::*;
        let keyword = match self {
            Strict => Keyword::Strict,
            StrictWithSize => Keyword::StrictWithSize,
            None => Keyword::None,
            Average => Keyword::Average,
            Fifo => Keyword::Fifo,
            Lifo => Keyword::Lifo,
            Hifo => Keyword::Hifo,
        };
        keyword.fmt_edn(f)
    }
}

impl FmtEdn for Date {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, r#"#time/date{SPACE}"{self}""#)
    }
}

impl FmtEdn for Decimal {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}M")
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

impl FmtEdn for parser::Flag {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        self.to_string().fmt_edn(f)
    }
}

#[derive(EnumString, EnumIter, IntoStaticStr, Clone, Debug)]
#[strum(serialize_all = "kebab-case")]
enum Keyword {
    Account,
    Average,
    Balance,
    Booking,
    Close,
    Comment,
    Commodity,
    Content,
    Cost,
    Currencies,
    Currency,
    Custom,
    Date,
    Description,
    Directive,
    Document,
    Event,
    Fifo,
    Flag,
    Hifo,
    Label,
    Lifo,
    Merge,
    Name,
    Narration,
    None,
    Note,
    Open,
    Pad,
    Path,
    Payee,
    PerUnit,
    Postings,
    Price,
    Query,
    Source,
    Strict,
    StrictWithSize,
    Tolerance,
    Txn,
    Type,
    Units,
    Values,
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
        f.write_str(VECTOR_BEGIN)?;

        for (item, sep) in self.0.zip(separators()) {
            if sep == Separator::Spaced {
                f.write_str(SPACE)?;
            }
            item.fmt_edn(f)?;
        }
        f.write_str(VECTOR_END)
    }
}

struct EdnSet<I, T>(I)
where
    I: Iterator<Item = T>;

impl<I, T> FmtEdn for EdnSet<I, T>
where
    I: Iterator<Item = T>,
    T: FmtEdn,
{
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(SET_BEGIN)?;

        for (item, sep) in self.0.zip(separators()) {
            if sep == Separator::Spaced {
                f.write_str(SPACE)?;
            }
            item.fmt_edn(f)?;
        }
        f.write_str(SET_END)
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
            f.write_str(COMMA_SPACE)?;
        }
        name.fmt_edn(f)?;
        f.write_str(SPACE)?;
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

/// TODO these being public is ugh
pub(crate) const VECTOR_BEGIN: &str = "[";
pub(crate) const VECTOR_END: &str = "]";

const SET_BEGIN: &str = "#{";
const SET_END: &str = "}";

// separators
const COMMA_SPACE: &str = ", ";
const SPACE: &str = " ";
const NEWLINE: &str = "\n";
