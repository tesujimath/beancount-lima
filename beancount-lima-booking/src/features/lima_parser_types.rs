use super::{Booking, Posting, Tolerance};
use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use time::Date;

impl<'a> Posting for &'a parser::Posting<'a> {
    type Date = time::Date;
    type Account = &'a str;
    type Currency = &'a str;
    type Number = Decimal;
    type Label = &'a str;

    fn account(&self) -> &'a str {
        parser::Posting::account(self).item().as_ref()
    }

    fn currency(&self) -> Option<&'a str> {
        parser::Posting::currency(self).map(|cur| cur.item().as_ref())
    }

    fn units(&self) -> Option<Decimal> {
        parser::Posting::amount(self).map(|amount| amount.item().value())
    }

    fn has_cost(&self) -> bool {
        self.cost_spec().is_some()
    }

    fn cost_currency(&self) -> Option<&'a str> {
        self.cost_spec().and_then(|cost_spec| {
            cost_spec
                .currency()
                .map(|currency| currency.item().as_ref())
        })
    }

    fn cost_per_unit(&self) -> Option<Decimal> {
        self.cost_spec()
            .and_then(|cost_spec| cost_spec.per_unit().map(|per_unit| per_unit.value()))
    }

    fn cost_total(&self) -> Option<Decimal> {
        self.cost_spec()
            .and_then(|cost_spec| cost_spec.total().map(|total| total.value()))
    }

    fn cost_date(&self) -> Option<Date> {
        self.cost_spec()
            .and_then(|cost_spec| cost_spec.date().map(|date| *date.item()))
    }

    fn cost_label(&self) -> Option<&'a str> {
        self.cost_spec()
            .and_then(|cost_spec| cost_spec.label().map(|label| label.item().as_ref()))
    }

    fn cost_merge(&self) -> Option<bool> {
        self.cost_spec().map(|cost_spec| cost_spec.merge())
    }

    fn has_price(&self) -> bool {
        self.price_annotation().is_some()
    }

    fn price_currency(&self) -> Option<&'a str> {
        self.price_annotation().and_then(|price| {
            use parser::PriceSpec::*;

            match price.item() {
                BareCurrency(currency) => Some(currency.as_ref()),
                CurrencyAmount(_, currency) => Some(currency.as_ref()),
                _ => None,
            }
        })
    }

    fn price_per_unit(&self) -> Option<Decimal> {
        self.price_annotation().and_then(|price| {
            use parser::PriceSpec::*;
            use parser::ScopedExprValue::*;

            match price.item() {
                BareAmount(PerUnit(expr)) => Some(expr.value()),
                CurrencyAmount(PerUnit(expr), _) => Some(expr.value()),
                _ => None,
            }
        })
    }

    fn price_total(&self) -> Option<Decimal> {
        self.price_annotation().and_then(|price| {
            use parser::PriceSpec::*;
            use parser::ScopedExprValue::*;

            match price.item() {
                BareAmount(Total(expr)) => Some(expr.value()),
                CurrencyAmount(Total(expr), _) => Some(expr.value()),
                _ => None,
            }
        })
    }
}

struct SumWithMinNonZeroScale {
    sum: Decimal,
    min_nonzero_scale: Option<u32>,
}

impl FromIterator<Decimal> for SumWithMinNonZeroScale {
    fn from_iter<T: IntoIterator<Item = Decimal>>(iter: T) -> Self {
        let mut sum = Decimal::ZERO;
        let mut min_nonzero_scale = None;
        for value in iter {
            sum += value;
            if value.scale() > 0 {
                if min_nonzero_scale.is_none() {
                    min_nonzero_scale = Some(value.scale());
                } else if let Some(scale) = min_nonzero_scale
                    && value.scale() < scale
                {
                    min_nonzero_scale = Some(value.scale());
                }
            }
        }

        Self {
            sum,
            min_nonzero_scale,
        }
    }
}

impl<'a> Tolerance for parser::Options<'a> {
    type Currency = &'a str;
    type Number = Decimal;

    // Beancount Precision & Tolerances
    // https://docs.google.com/document/d/1lgHxUUEY-UVEgoF6cupz2f_7v7vEF7fiJyiSlYYlhOo
    fn residual(
        &self,
        values: impl Iterator<Item = Self::Number>,
        cur: &Self::Currency,
    ) -> Option<Self::Number> {
        let multiplier = self
            .inferred_tolerance_multiplier()
            .map(|m| *m.item())
            .unwrap_or(default_inferred_tolerance_multiplier());
        let s = values.collect::<SumWithMinNonZeroScale>();
        let residual = s.sum;
        let abs_residual = residual.abs();

        if let Some(min_nonzero_scale) = s.min_nonzero_scale.as_ref() {
            (abs_residual >= Decimal::new(1, *min_nonzero_scale) * multiplier).then_some(residual)
        } else {
            // TODO should we have kept currency as a parser::Currency all along, to avoid extra validation here??
            let cur = TryInto::<parser::Currency>::try_into(*cur).unwrap();
            let tolerance = self
                .inferred_tolerance_default(&cur)
                .or(self.inferred_tolerance_default_fallback());

            if let Some(tolerance) = tolerance {
                (abs_residual >= tolerance).then_some(residual)
            } else {
                (!residual.is_zero()).then_some(residual)
            }
        }
    }
}

impl From<parser::Booking> for Booking {
    fn from(value: parser::Booking) -> Self {
        use parser::Booking as parser;
        use Booking::*;

        match value {
            parser::Strict => Strict,
            parser::StrictWithSize => StrictWithSize,
            parser::None => None,
            parser::Average => Average,
            parser::Fifo => Fifo,
            parser::Lifo => Lifo,
            parser::Hifo => Hifo,
        }
    }
}

// TODO where should default_inferred_tolerance_multiplier be defined?
// (we can't depend on the main beancount-lima crate here)
fn default_inferred_tolerance_multiplier() -> Decimal {
    Decimal::new(5, 1) // 0.5
}
