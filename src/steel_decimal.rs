use std::ops::Add;

use steel::{SteelErr, SteelVal};

use crate::{custom_wrapper::CustomWrapperWithEq, steely::Steely};

pub(crate) type SteelDecimal = Steely<CustomWrapperWithEq<rust_decimal::Decimal>>;

impl SteelDecimal {
    pub(crate) fn add(&self, other: SteelDecimal) -> SteelDecimal {
        (self.as_ref() + other.as_ref()).into()
    }

    pub(crate) fn to_rational(self) -> steel::rvals::Result<SteelVal> {
        let numerator = self.mantissa();
        let denominator = 10i128.pow(self.scale());

        match (i32::try_from(numerator), i32::try_from(denominator)) {
            (Ok(numerator), Ok(denominator)) => {
                Ok(SteelVal::Rational((numerator, denominator).into()))
            }
            _ => Err(SteelErr::new(
                steel::rerrs::ErrorKind::ConversionError,
                "overflow, BigRatio not yet supported".to_string(),
            )),
        }
    }

    pub(crate) fn new(m: i64, e: u32) -> Self {
        rust_decimal::Decimal::new(m, e).into()
    }

    pub(crate) fn zero() -> Self {
        rust_decimal::Decimal::ZERO.into()
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.as_ref().is_zero()
    }

    // width of digits and/or sign to left of decimal point
    pub(crate) fn width_left(&self) -> u32 {
        let sign_width = if self.is_sign_negative() { 1u32 } else { 0 };
        let mut mantissa_width = 0u32;
        let mut abs_mantissa = self.mantissa().abs();
        while abs_mantissa > 0 {
            abs_mantissa /= 10;
            mantissa_width += 1;
        }

        if sign_width + mantissa_width > self.scale() {
            sign_width + mantissa_width - self.scale()
        } else {
            1
        }
    }

    // width of digits  to right of decimal point
    pub(crate) fn width_right(&self) -> u32 {
        self.scale()
    }

    pub(crate) fn parse(raw: String) -> steel::rvals::Result<Self> {
        rust_decimal::Decimal::from_str_exact(&raw)
            .map_err(|e| SteelErr::new(steel::rerrs::ErrorKind::ConversionError, e.to_string()))
            .map(Into::into)
    }

    /// parse a decimal forcing at least 2 decimal places
    pub(crate) fn parse_cents(raw: String) -> steel::rvals::Result<Self> {
        Self::parse(raw).map(|mut d| {
            let d = d.as_mut();
            if d.scale() < 2 {
                d.rescale(2);
            }
            (*d).into()
        })
    }
}

impl Add for &SteelDecimal {
    type Output = SteelDecimal;

    fn add(self, rhs: &SteelDecimal) -> SteelDecimal {
        self.map2(rhs, |value, rhs| *value + *rhs)

        // let value = self.value.get_mut().unwrap().get_mut();
        // let value = value.as_any_ref_mut().downcast_mut::<T>().unwrap();
        // let rhs = rhs.value.read();
        // let rhs = rhs.as_any_ref().downcast_ref::<T>().unwrap();
        // value.add_assign(*rhs);
    }
}
