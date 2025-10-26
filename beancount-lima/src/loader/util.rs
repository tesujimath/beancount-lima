#![allow(dead_code, unused_variables)]
use beancount_parser_lima as parser;

pub(crate) fn cost_spec_currency<'a>(
    cost: &'a parser::CostSpec<'a>,
) -> Option<&'a parser::Currency<'a>> {
    cost.currency().map(parser::Spanned::item)
}

pub(crate) fn price_spec_currency<'a>(
    price: &'a parser::PriceSpec<'a>,
) -> Option<&'a parser::Currency<'a>> {
    use parser::PriceSpec::*;

    match price {
        BareCurrency(currency) => Some(currency),
        CurrencyAmount(_, currency) => Some(currency),
        _ => None,
    }
}
