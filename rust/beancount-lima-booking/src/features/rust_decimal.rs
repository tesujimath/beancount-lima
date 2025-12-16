use super::{Number, Sign};

impl Number for rust_decimal::Decimal {
    fn abs(&self) -> Self {
        rust_decimal::Decimal::abs(self)
    }

    fn sign(&self) -> Option<Sign> {
        use Sign::*;

        if self.is_zero() {
            None
        } else if self.is_sign_negative() {
            Some(Negative)
        } else {
            Some(Positive)
        }
    }

    fn zero() -> Self {
        rust_decimal::Decimal::ZERO
    }

    fn scale(&self) -> u32 {
        self.scale()
    }

    fn rescaled(self, scale: u32) -> Self {
        let mut n = self;
        n.rescale(scale);
        n
    }
}
