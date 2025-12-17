use rust_decimal::Decimal;
use time::Date;
use tracing::Instrument;

use crate::import::{Context, Import, Source};
use crate::loader::*;
use beancount_parser_lima as parser;
use color_eyre::eyre::Result;
use std::fmt::{self, Display, Formatter, Write};
use std::iter::{empty, once, repeat};
use strum_macros::{EnumIter, EnumString, IntoStaticStr};

pub(crate) fn write_booked_as_edn<'a, W>(
    directives: &[Directive<'a>],
    options: &parser::Options,
    out_w: W,
) -> Result<()>
where
    W: std::io::Write + Copy,
{
    use std::io::{BufWriter, Write};

    let mut buffered_out_w = BufWriter::new(out_w);

    // TODO tidy up writing a large map
    writeln!(
        buffered_out_w,
        "{MAP_BEGIN}\n{} {VECTOR_BEGIN}",
        Edn(Keyword::Directives)
    )?;

    for d in directives {
        writeln!(buffered_out_w, "{}", Edn(d))?;
    }

    writeln!(buffered_out_w, "{VECTOR_END}")?;

    writeln!(buffered_out_w, "{} {}", Edn(Keyword::Options), Edn(options))?;

    writeln!(buffered_out_w, "{MAP_END}")?;

    Ok(())
}

pub(crate) fn write_import_as_edn<W>(import: &Import, out_w: W) -> Result<()>
where
    W: std::io::Write + Copy,
{
    use std::io::{BufWriter, Write};

    let mut buffered_out_w = BufWriter::new(out_w);
    writeln!(buffered_out_w, "{}\n", Edn(import))?;

    Ok(())
}

// TODO improve this, it's a bit ugly
struct Edn<T>(T)
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

trait FmtEdn {
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
        (Keyword::Directive, Keyword::Transaction, Spaced).fmt_edn(f)?;
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
        (Keyword::Directive, Keyword::Transaction, Spaced).fmt_edn(f)?;
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
        let mut currencies = parsed.currencies().peekable();
        if currencies.peek().is_some() {
            (
                Keyword::Currencies,
                EdnSet(currencies.map(|cur| *cur.item())),
                Spaced,
            )
                .fmt_edn(f)?;
        }
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
        if self.merge {
            (Keyword::Merge, self.merge, Spaced).fmt_edn(f)?;
        }
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

impl FmtEdn for parser::PluginProcessingMode {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use beancount_parser_lima::PluginProcessingMode::*;
        let keyword = match self {
            Default => Keyword::Default,
            Raw => Keyword::Raw,
        };
        keyword.fmt_edn(f)
    }
}

// plugin_processing_mode(&self) -> Option<&Spanned<PluginProcessingMode>> {
//
impl<'a> FmtEdn for &parser::Options<'a> {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (
            Keyword::NameAssets,
            self.account_type_name(parser::AccountType::Assets).as_ref(),
            Flush,
        )
            .fmt_edn(f)?;
        (
            Keyword::NameLiabilities,
            self.account_type_name(parser::AccountType::Liabilities)
                .as_ref(),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::NameEquity,
            self.account_type_name(parser::AccountType::Equity).as_ref(),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::NameIncome,
            self.account_type_name(parser::AccountType::Income).as_ref(),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::NameExpenses,
            self.account_type_name(parser::AccountType::Expenses)
                .as_ref(),
            Spaced,
        )
            .fmt_edn(f)?;
        if let Some(title) = self.title() {
            (Keyword::Title, *title.item(), Spaced).fmt_edn(f)?;
        }
        if let Some(account_previous_balances) = self.account_previous_balances() {
            (
                Keyword::AccountPreviousBalances,
                account_previous_balances.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_previous_earnings) = self.account_previous_earnings() {
            (
                Keyword::AccountPreviousEarnings,
                account_previous_earnings.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_previous_conversions) = self.account_previous_conversions() {
            (
                Keyword::AccountPreviousConversions,
                account_previous_conversions.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_current_earnings) = self.account_current_earnings() {
            (
                Keyword::AccountCurrentEarnings,
                account_current_earnings.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_current_conversions) = self.account_current_conversions() {
            (
                Keyword::AccountCurrentConversions,
                account_current_conversions.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_unrealized_gains) = self.account_unrealized_gains() {
            (
                Keyword::AccountUnrealizedGains,
                account_unrealized_gains.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(account_rounding) = self.account_rounding() {
            (
                Keyword::AccountRounding,
                account_rounding.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        if let Some(conversion_currency) = self.conversion_currency() {
            (
                Keyword::ConversionCurrency,
                conversion_currency.item().as_ref(),
                Spaced,
            )
                .fmt_edn(f)?;
        }

        let mut inferred_tolerance_defaults = self.inferred_tolerance_defaults().peekable();
        if inferred_tolerance_defaults.peek().is_some() {
            write!(
                f,
                "{COMMA_SPACE}{}{SPACE}",
                Edn(Keyword::InferredToleranceDefault)
            )?;
            map_begin(f)?;
            for ((cur, tol), sep) in inferred_tolerance_defaults.zip(separators()) {
                if let Some(cur) = cur {
                    (cur, tol, sep).fmt_edn(f)?;
                } else {
                    (ANY, tol, sep).fmt_edn(f)?;
                }
            }
            map_end(f)?;
        }

        if let Some(inferred_tolerance_multiplier) = self.inferred_tolerance_multiplier() {
            (
                Keyword::InferredToleranceMultiplier,
                *inferred_tolerance_multiplier.item(),
                Spaced,
            )
                .fmt_edn(f)?;
        }

        if let Some(infer_tolerance_from_cost) = self.infer_tolerance_from_cost() {
            (
                Keyword::InferToleranceFromCost,
                *infer_tolerance_from_cost.item(),
                Spaced,
            )
                .fmt_edn(f)?;
        }

        let mut documents = self.documents().peekable();
        if documents.peek().is_some() {
            write!(f, "{COMMA_SPACE}{}{SPACE}", Edn(Keyword::Documents))?;
            let documents = documents
                .map(|doc| doc.to_string_lossy())
                .collect::<Vec<_>>();
            EdnVector(documents.iter().map(|doc| doc.as_ref())).fmt_edn(f)?;
        }

        let mut operating_currency = self.operating_currency().peekable();
        if operating_currency.peek().is_some() {
            write!(f, "{COMMA_SPACE}{}{SPACE}", Edn(Keyword::OperatingCurrency))?;
            EdnSet(operating_currency.map(|cur| cur.as_ref())).fmt_edn(f)?;
        }

        if let Some(render_commas) = self.render_commas() {
            (Keyword::RenderCommas, *render_commas.item(), Spaced).fmt_edn(f)?;
        }
        if let Some(booking_method) = self.booking_method() {
            (Keyword::Booking, *booking_method.item(), Spaced).fmt_edn(f)?;
        }
        if let Some(plugin_processing_mode) = self.plugin_processing_mode() {
            (
                Keyword::PluginProcessingMode,
                *plugin_processing_mode.item(),
                Spaced,
            )
                .fmt_edn(f)?;
        }
        map_end(f)
    }
}

impl FmtEdn for &Import {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (Keyword::Sources, EdnVector(self.sources.iter()), Flush).fmt_edn(f)?;
        if let Some(context) = self.context.as_ref() {
            (Keyword::Context, context, Spaced).fmt_edn(f)?;
        }
        map_end(f)
    }
}

impl FmtEdn for &Source {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;
        (
            Keyword::Header,
            EdnMap(self.header.iter().map(|(k, v)| (*k, v.as_str()))),
            Flush,
        )
            .fmt_edn(f)?;
        (
            Keyword::Fields,
            EdnVector(self.fields.iter().map(|v| v.as_str())),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::Transactions,
            EdnVector(
                self.transactions
                    .iter()
                    .map(|vs| EdnVector(vs.iter().map(|v| v.as_str()))),
            ),
            Spaced,
        )
            .fmt_edn(f)?;
        map_end(f)
    }
}

impl FmtEdn for &Context {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        use Separator::*;

        map_begin(f)?;

        (Keyword::Path, self.path.as_str(), Flush).fmt_edn(f)?;
        (
            Keyword::Txnids,
            EdnSet(self.txnids.iter().map(|x| x.as_str())),
            Spaced,
        )
            .fmt_edn(f)?;
        (
            Keyword::Payees,
            EdnMap(
                self.payees
                    .iter()
                    .map(|(x, m)| (x.as_str(), EdnMap(m.iter().map(|(k, v)| (k.as_str(), *v))))),
            ),
            Flush,
        )
            .fmt_edn(f)?;
        (
            Keyword::Narrations,
            EdnMap(
                self.narrations
                    .iter()
                    .map(|(x, m)| (x.as_str(), EdnMap(m.iter().map(|(k, v)| (k.as_str(), *v))))),
            ),
            Flush,
        )
            .fmt_edn(f)?;
        map_end(f)
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

impl FmtEdn for usize {
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
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

// ubiquitous keywords, e.g. currency, are abbreviated
#[derive(EnumString, EnumIter, IntoStaticStr, Clone, Debug)]
#[strum(serialize_all = "kebab-case")]
enum Keyword {
    #[strum(to_string = "acc")]
    Account,
    AccountCurrentConversions,
    AccountCurrentEarnings,
    AccountPreviousBalances,
    AccountPreviousConversions,
    AccountPreviousEarnings,
    AccountRounding,
    AccountUnrealizedGains,
    Average,
    Balance,
    Booking,
    Close,
    Comment,
    Commodity,
    Content,
    Context,
    ConversionCurrency,
    Cost,
    Currencies,
    #[strum(to_string = "cur")]
    Currency,
    Custom,
    Date,
    Default,
    Description,
    #[strum(to_string = "dct")]
    Directive,
    Directives,
    Document,
    Documents,
    Event,
    Fields,
    Fifo,
    Flag,
    Header,
    Hifo,
    InferToleranceFromCost,
    InferredToleranceDefault,
    InferredToleranceMultiplier,
    Label,
    Lifo,
    Merge,
    Name,
    NameAssets,
    NameEquity,
    NameExpenses,
    NameIncome,
    NameLiabilities,
    Narration,
    Narrations,
    None,
    Note,
    Open,
    OperatingCurrency,
    Options,
    Pad,
    Path,
    Payee,
    Payees,
    PerUnit,
    PluginProcessingMode,
    Postings,
    Price,
    Query,
    Raw,
    RenderCommas,
    Source,
    Sources,
    Strict,
    StrictWithSize,
    Title,
    Tolerance,
    #[strum(to_string = "txn")]
    Transaction,
    Transactions,
    Txnids,
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

// a homgeneous map
struct EdnMap<I, K, V>(I)
where
    I: Iterator<Item = (K, V)>;

impl<I, K, V> FmtEdn for EdnMap<I, K, V>
where
    I: Iterator<Item = (K, V)>,
    K: FmtEdn,
    V: FmtEdn,
{
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(MAP_BEGIN)?;

        for ((k, v), sep) in self.0.zip(separators()) {
            (k, v, sep).fmt_edn(f)?;
        }
        f.write_str(MAP_END)
    }
}

// a map entry with optional separator prefix
//
// we can't implement map formatting like we did with sets and vectors because of the heterogeny
impl<K, V> FmtEdn for (K, V, Separator)
where
    K: FmtEdn,
    V: FmtEdn,
{
    fn fmt_edn(self, f: &mut Formatter<'_>) -> fmt::Result {
        let (key, value, sep) = self;
        if sep == Separator::Spaced {
            f.write_str(COMMA_SPACE)?;
        }
        key.fmt_edn(f)?;
        f.write_str(SPACE)?;
        value.fmt_edn(f)
    }
}

fn map_begin(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str(MAP_BEGIN)
}

fn map_end(f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str(MAP_END)
}

// an infinite iterator of separators, with the first one only being flush
fn separators() -> impl Iterator<Item = Separator> {
    use Separator::*;

    once(Flush).chain(repeat(Spaced))
}

const VECTOR_BEGIN: &str = "[";
const VECTOR_END: &str = "]";
const MAP_BEGIN: &str = "{";
const MAP_END: &str = "}";
const SET_BEGIN: &str = "#{";
const SET_END: &str = "}";

// separators
const COMMA_SPACE: &str = ", ";
const SPACE: &str = " ";
const NEWLINE: &str = "\n";

const ANY: &str = "*";
