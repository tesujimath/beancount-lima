use super::Posting;
use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use time::Date;

impl<'a> Posting<Date, &'a str, Decimal, &'a str, &'a str> for &'a parser::Posting<'a> {
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
