use super::Number;

impl Number for rust_decimal::Decimal {
    fn abs(&self) -> Self {
        rust_decimal::Decimal::abs(self)
    }
}
