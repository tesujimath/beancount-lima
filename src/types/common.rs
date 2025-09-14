// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::fmt::Display;
use steel::{
    rvals::{as_underlying_type, Custom, CustomType, SteelString},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};

use crate::types::{steel_date::SteelDate, steel_decimal::SteelDecimal};

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Posting {
    pub(crate) flag: Option<SteelString>,
    pub(crate) account: SteelString,
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
            account: account.to_string().into(),
            amount,
            flag: flag.map(|flag| flag.to_string().into()),
        }
    }

    fn account(&self) -> SteelString {
        self.account.clone()
    }

    fn amount(&self) -> Amount {
        self.amount.clone()
    }

    fn flag(&self) -> Option<SteelString> {
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
    pub(crate) number: SteelDecimal,
    pub(crate) currency: SteelString,
}

// https://github.com/mattwparas/steel/issues/365
impl Amount {
    fn new(number: SteelDecimal, currency: SteelString) -> Self {
        Self { number, currency }
    }

    fn number(&self) -> SteelDecimal {
        self.number
    }

    fn currency(&self) -> SteelString {
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

impl<S> From<(Decimal, S)> for Amount
where
    S: Display,
{
    fn from(value: (Decimal, S)) -> Self {
        Amount {
            number: value.0.into(),
            currency: value.1.to_string().into(),
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
    number: SteelDecimal,
    currency: SteelString,
    date: SteelDate,
    label: Option<SteelString>,
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
}
