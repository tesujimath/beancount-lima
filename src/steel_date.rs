use std::{fmt::Display, ops::Deref};
use steel::{
    rvals::Custom,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr,
};
use time::Date;

#[derive(Clone, Debug)]
pub struct SteelDate(Date);

impl SteelDate {
    fn new(y: i32, m: i32, d: i32) -> steel::rvals::Result<Self> {
        u8::try_from(m)
            .ok()
            .and_then(|m| time::Month::try_from(m).ok())
            .and_then(|m| u8::try_from(d).ok().map(|d| (m, d)))
            .and_then(|(m, d)| Date::from_calendar_date(y, m, d).ok())
            .map(|date| date.into())
            .ok_or(SteelErr::new(
                steel::rerrs::ErrorKind::ConversionError,
                "bad date".to_string(),
            ))
    }

    fn parse(raw: String, strftime_format: String) -> steel::rvals::Result<Self> {
        let format_descr = time::format_description::parse_strftime_borrowed(&strftime_format)
            .map_err(|e| {
                SteelErr::new(
                    steel::rerrs::ErrorKind::ConversionError,
                    format!("bad date format: {e}"),
                )
            })?;
        Date::parse(&raw, &format_descr)
            .map_err(|e| {
                SteelErr::new(
                    steel::rerrs::ErrorKind::ConversionError,
                    format!("bad date: {e}"),
                )
            })
            .map(Self)
    }

    // beginning of time
    fn bot() -> SteelDate {
        Date::MIN.into()
    }

    // end of time
    fn eot() -> SteelDate {
        Date::MAX.into()
    }

    // positive or negative offset in days
    fn after(&self, d: isize) -> steel::rvals::Result<Self> {
        if d >= 0 {
            self.0.checked_add(time::Duration::days(d as i64))
        } else {
            self.0.checked_sub(time::Duration::days(-d as i64))
        }
        .map_or(
            Err(SteelErr::new(
                steel::rerrs::ErrorKind::ConversionError,
                "date overflow".to_string(),
            )),
            |date| Ok(date.into()),
        )
    }

    // positive or negative offset in days
    fn before(&self, d: isize) -> steel::rvals::Result<Self> {
        self.after(-d)
    }

    // Julian day
    fn julian(&self) -> i32 {
        self.0.to_julian_day()
    }
}

impl Custom for SteelDate {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }
}

impl Copy for SteelDate {}

impl From<Date> for SteelDate {
    fn from(value: Date) -> Self {
        SteelDate(value)
    }
}

impl PartialEq for SteelDate {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl PartialOrd for SteelDate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for SteelDate {}

impl Ord for SteelDate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl Display for SteelDate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for SteelDate {
    type Target = Date;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<SteelDate>("date?");
    steel_engine.register_fn("date", SteelDate::new);
    steel_engine.register_fn("date-bot", SteelDate::bot);
    steel_engine.register_fn("date-eot", SteelDate::eot);
    steel_engine.register_fn("date=?", SteelDate::eq);
    steel_engine.register_fn("date>?", SteelDate::gt);
    steel_engine.register_fn("date<?", SteelDate::lt);
    steel_engine.register_fn("date>=?", SteelDate::ge);
    steel_engine.register_fn("date<=?", SteelDate::le);
    steel_engine.register_fn("parse-date", SteelDate::parse);
    steel_engine.register_fn("date-after", SteelDate::after);
    steel_engine.register_fn("date-before", SteelDate::before);
    steel_engine.register_fn("date-julian", SteelDate::julian);
}
