use rust_decimal::Decimal;
use std::{
    fmt::Display,
    ops::{Add, AddAssign},
};
use steel::{
    rvals::{as_underlying_type, Custom, CustomType},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr,
};

#[derive(Copy, Clone, Debug)]
pub struct SteelDecimal(Decimal);

impl SteelDecimal {
    fn new(m: i64, e: u32) -> Self {
        Decimal::new(m, e).into()
    }

    fn add(&self, other: SteelDecimal) -> SteelDecimal {
        (self.0 + other.0).into()
    }

    fn numerator(&self) -> isize {
        self.0.mantissa() as isize
    }

    fn denominator(&self) -> isize {
        10isize.pow(self.0.scale())
    }

    fn zero() -> Self {
        Decimal::ZERO.into()
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.0 == Decimal::ZERO
    }

    // width of digits and/or sign to left of decimal point
    fn width_left(&self) -> u32 {
        let sign_width = if self.0.is_sign_negative() { 1u32 } else { 0 };
        let mut mantissa_width = 0u32;
        let mut abs_mantissa = self.0.mantissa().abs();
        while abs_mantissa > 0 {
            abs_mantissa /= 10;
            mantissa_width += 1;
        }

        if sign_width + mantissa_width > self.0.scale() {
            sign_width + mantissa_width - self.0.scale()
        } else {
            1
        }
    }

    // width of digits  to right of decimal point
    fn width_right(&self) -> u32 {
        self.0.scale()
    }

    fn parse(raw: String) -> steel::rvals::Result<Self> {
        Decimal::from_str_exact(&raw)
            .map_err(|e| SteelErr::new(steel::rerrs::ErrorKind::ConversionError, e.to_string()))
            .map(Self)
    }

    /// parse a decimal forcing at least 2 decimal places
    fn parse_cents(raw: String) -> steel::rvals::Result<Self> {
        Self::parse(raw).map(|Self(mut d)| {
            if d.scale() < 2 {
                d.rescale(2);
            }
            Self(d)
        })
    }
}

impl From<Decimal> for SteelDecimal {
    fn from(value: Decimal) -> Self {
        Self(value)
    }
}

impl From<SteelDecimal> for Decimal {
    fn from(value: SteelDecimal) -> Self {
        value.0
    }
}

impl PartialEq for SteelDecimal {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Eq for SteelDecimal {}

impl PartialOrd for SteelDecimal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Ord for SteelDecimal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl Display for SteelDecimal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Custom for SteelDecimal {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.0.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelDecimal>(other) {
            self == other
        } else {
            false
        }
    }
}

impl Add for SteelDecimal {
    type Output = SteelDecimal;

    fn add(self, rhs: Self) -> Self::Output {
        (self.0 + rhs.0).into()
    }
}

impl AddAssign for SteelDecimal {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<SteelDecimal>("decimal?");
    steel_engine.register_fn("decimal", SteelDecimal::new);
    steel_engine.register_fn("decimal=?", SteelDecimal::eq);
    steel_engine.register_fn("decimal>?", SteelDecimal::gt);
    steel_engine.register_fn("decimal<?", SteelDecimal::lt);
    steel_engine.register_fn("decimal>=?", SteelDecimal::ge);
    steel_engine.register_fn("decimal<=?", SteelDecimal::le);
    steel_engine.register_fn("decimal-zero", SteelDecimal::zero);
    steel_engine.register_fn("decimal-zero?", SteelDecimal::is_zero);
    steel_engine.register_fn("decimal-add", SteelDecimal::add);
    steel_engine.register_fn("decimal->string", SteelDecimal::to_string);
    steel_engine.register_fn("decimal-numerator", SteelDecimal::numerator);
    steel_engine.register_fn("decimal-denominator", SteelDecimal::denominator);
    steel_engine.register_fn("decimal-width-left", SteelDecimal::width_left);
    steel_engine.register_fn("decimal-width-right", SteelDecimal::width_right);
    steel_engine.register_fn("parse-decimal", SteelDecimal::parse);
    steel_engine.register_fn("parse-decimal-cents", SteelDecimal::parse_cents);
}
