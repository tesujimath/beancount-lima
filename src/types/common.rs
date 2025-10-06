// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::fmt::Display;
use steel::{
    rvals::{as_underlying_type, Custom, CustomType},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};
use tabulator::{Align, Cell, Gap};
use time::Date;

use crate::types::steel_decimal::SteelDecimal;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Posting {
    pub(crate) flag: Option<String>,
    pub(crate) account: String,
    pub(crate) amount: Amount,
    // TODO:
    // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    // pub(crate) price_annotation: Option<Spanned<PriceSpec<'a>>>,
    // pub(crate) metadata: Metadata<'a>,
}

impl Posting {
    pub(crate) fn new<S1, S2>(account: S1, amount: Amount, flag: Option<S2>) -> Self
    where
        S1: Display,
        S2: Display,
    {
        Posting {
            account: account.to_string(),
            amount,
            flag: flag.map(|flag| flag.to_string()),
        }
    }

    fn account(&self) -> String {
        self.account.clone()
    }

    fn amount(&self) -> Amount {
        self.amount.clone()
    }

    fn flag(&self) -> Option<String> {
        self.flag.clone()
    }
}

impl Custom for Posting {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Posting>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Amount {
    pub(crate) number: Decimal,
    pub(crate) currency: String,
}

// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn new(number: SteelDecimal, currency: String) -> Self {
        Self {
            number: number.into(),
            currency,
        }
    }

    fn number(&self) -> SteelDecimal {
        self.number.into()
    }

    fn currency(&self) -> String {
        self.currency.clone()
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.number, &self.currency)
    }
}

impl Custom for Amount {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Amount>(other) {
            self == other
        } else {
            false
        }
    }
}

impl From<Amount> for Cell<'static> {
    fn from(value: Amount) -> Self {
        Cell::Row(
            vec![value.number.into(), (value.currency, Align::Left).into()],
            Gap::Minor,
        )
    }
}

impl<S> From<(Decimal, S)> for Amount
where
    S: Display,
{
    fn from(value: (Decimal, S)) -> Self {
        Amount {
            number: value.0,
            currency: value.1.to_string(),
        }
    }
}

impl From<&parser::Amount<'_>> for Amount {
    fn from(value: &parser::Amount) -> Self {
        (value.number().value(), value.currency()).into()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Cost {
    number: Decimal,
    currency: String,
    date: Date,
    label: Option<String>,
}

impl Display for Cost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", &self.number, &self.currency, &self.date)?;
        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        Ok(())
    }
}

impl Custom for Cost {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Cost>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Position {
    pub(crate) units: Amount,
    pub(crate) cost: Option<Cost>,
}

impl Position {
    fn new(units: Amount, cost: Option<Cost>) -> Self {
        Self { units, cost }
    }

    fn units(&self) -> Amount {
        self.units.clone()
    }

    fn cost(&self) -> Option<Cost> {
        self.cost.clone()
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.units)?;
        if let Some(cost) = &self.cost {
            write!(f, " {{ {cost} }}")?;
        }

        Ok(())
    }
}

impl Custom for Position {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Position>(other) {
            self == other
        } else {
            false
        }
    }
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<Posting>("posting?");
    steel_engine.register_fn("posting->string", Posting::to_string);
    steel_engine.register_fn("posting-account", Posting::account);
    steel_engine.register_fn("posting-amount", Posting::amount);
    steel_engine.register_fn("posting-flag", Posting::flag);

    steel_engine.register_type::<Amount>("amount?");
    steel_engine.register_fn("amount", Amount::new);
    steel_engine.register_fn("amount->string", Amount::to_string);
    steel_engine.register_fn("amount-number", Amount::number);
    steel_engine.register_fn("amount-currency", Amount::currency);

    steel_engine.register_type::<Position>("position?");
    steel_engine.register_fn("position", Position::new);
    steel_engine.register_fn("position-units", Position::units);
    steel_engine.register_fn("position-cost", Position::cost);
}
