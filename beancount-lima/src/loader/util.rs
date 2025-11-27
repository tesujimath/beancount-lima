// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use beancount_parser_lima as parser;

pub(crate) fn cost_spec_currency<'a>(
    cost: &'a parser::CostSpec<'a>,
) -> Option<parser::Currency<'a>> {
    cost.currency().map(|cur| *cur.item())
}

pub(crate) fn price_spec_currency<'a>(
    price: &'a parser::PriceSpec<'a>,
) -> Option<parser::Currency<'a>> {
    use parser::PriceSpec::*;

    match price {
        BareCurrency(cur) => Some(*cur),
        CurrencyAmount(_, cur) => Some(*cur),
        _ => None,
    }
}
