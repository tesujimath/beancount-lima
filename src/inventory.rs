// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima as parser;
use joinery::JoinableIterator;
use rust_decimal::Decimal;
use std::{collections::HashMap, fmt::Display, ops::Deref};
use steel::{
    gc::Gc,
    rvals::{as_underlying_type, Custom, CustomType, IntoSteelVal, SteelString, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal, Vector,
};
use steel_derive::Steel;
use strum_macros::{Display, EnumIter, EnumString, IntoStaticStr};
use time::Date;

use crate::{booking::*, steel_date::SteelDate, steel_decimal::SteelDecimal, types::*};

impl From<Cost> for SteelCost {
    fn from(value: Cost) -> Self {
        let Cost {
            number,
            currency,
            date,
            label,
        } = value;
        Self {
            amount: (number, currency).into(),
            date: date.into(),
            label: label.map(|label| label.into()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SteelCost {
    amount: SteelAmount,
    date: SteelDate,
    label: Option<SteelString>,
}

impl Display for SteelCost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &self.amount, &self.date)?;
        if let Some(label) = &self.label {
            write!(f, " {label}")?;
        }

        Ok(())
    }
}

impl Custom for SteelCost {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelCost>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SteelPosition {
    units: SteelAmount,
    cost: Option<SteelCost>,
}

impl From<(String, Position)> for SteelPosition {
    fn from(value: (String, Position)) -> Self {
        let (currency, Position { units, cost }) = value;
        Self {
            units: (units, currency).into(),
            cost: cost.map(|cost| cost.into()),
        }
    }
}

impl Display for SteelPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.units)?;
        if let Some(cost) = &self.cost {
            write!(f, " {{ {cost} }}")?;
        }

        Ok(())
    }
}

impl Custom for SteelPosition {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelPosition>(other) {
            self == other
        } else {
            false
        }
    }
}

// TODO include commodities held at cost
#[derive(Clone, Steel, Default, Debug)]
pub(crate) struct Inventory(HashMap<String, Positions>); // indexed by currency

impl Deref for Inventory {
    type Target = HashMap<String, Positions>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Inventory {
    fn with_initial_position(currency: &str, position: Position) -> Inventory {
        let mut positions = HashMap::default();
        positions.insert(currency.to_string(), position.into());

        Self(positions)
    }

    // TODO this should include CostSpec and the booking method
    fn book(&mut self, currency: &str, position: Position) {
        // TODO cost
        match self.0.get_mut(currency) {
            Some(positions) => positions.book(position, Booking::Strict), // TODO booking method
            None => {
                self.0.insert(currency.to_string(), position.into());
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SteelInventory(SteelVector); // of SteelPosition

impl From<Inventory> for SteelInventory {
    fn from(value: Inventory) -> Self {
        // return positions sorted by currency
        let Inventory(mut positions_by_currency) = value;
        // sorted by currency for determinism
        let mut currencies = positions_by_currency
            .keys()
            .map(|currency| currency.to_string())
            .collect::<Vec<_>>();
        currencies.sort();

        let positions = currencies
            .into_iter()
            .flat_map(|currency| {
                positions_by_currency
                    .remove(&currency)
                    .unwrap()
                    .into_iter_with_currency(currency)
                    .map(Into::<SteelPosition>::into)
                    .map(|position| position.into_steelval().unwrap())
            })
            .collect::<Vec<_>>();

        SteelInventory(Gc::new(Into::<Vector<_>>::into(positions)).into())
    }
}

impl Display for SteelInventory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join_with(" "))
    }
}

impl Custom for SteelInventory {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SteelInventory>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, Default, Debug)]
pub(crate) struct InventoryAccumulator {
    accounts: HashMap<String, Inventory>, // indexed by account name
    currency_usage: HashMap<String, i32>,
}

impl Custom for InventoryAccumulator {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", &self)))
    }
}

impl InventoryAccumulator {
    pub(crate) fn post(&mut self, posting: SteelVal) {
        if let SteelVal::Custom(posting) = posting {
            if let Some(posting) = as_underlying_type::<SteelPosting>(posting.read().as_ref()) {
                let account_name = posting.account.as_str();
                let units = posting.amount.number.into();
                let currency = posting.amount.currency.as_str();
                // TODO cost
                let position = Position { units, cost: None };

                match self.accounts.get_mut(account_name) {
                    Some(account) => account.book(currency, position),
                    None => {
                        self.accounts.insert(
                            account_name.to_string(),
                            Inventory::with_initial_position(currency, position),
                        );
                    }
                }

                match self.currency_usage.get_mut(currency) {
                    Some(usage) => {
                        *usage += 1;
                    }
                    None => {
                        self.currency_usage.insert(currency.to_string(), 1);
                    }
                }
            } else {
                panic!("InventoryBuilder::post with something other than a Posting")
            }
        } else {
            panic!("InventoryBuilder::post with something other than a Custom")
        }
    }

    fn currencies(&self) -> Vec<SteelString> {
        self.currency_usage
            .keys()
            .map(|currency| currency.to_string().into())
            .collect::<Vec<SteelString>>()
    }

    fn main_currency(&self) -> SteelString {
        let main_currency = self
            .currency_usage
            .iter()
            .max_by_key(|(_, n)| **n)
            .map(|(cur, _)| cur.clone())
            .unwrap_or(DEFAULT_CURRENCY.to_string());

        main_currency.into()
    }

    fn build(self) -> SteelVal {
        let Self { accounts, .. } = self;

        SteelVal::HashMapV(
            Gc::new(
                accounts
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            k.into(),
                            Into::<SteelInventory>::into(v).into_steelval().unwrap(),
                        )
                    })
                    .collect::<steel::HashMap<SteelVal, SteelVal>>(),
            )
            .into(),
        )
    }
}

const DEFAULT_CURRENCY: &str = "USD"; // ugh

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<InventoryAccumulator>("inventory-accumulator?");
    steel_engine.register_fn("inventory-accumulator", InventoryAccumulator::default);
    steel_engine.register_fn("inventory-accumulator-post", InventoryAccumulator::post);
    steel_engine.register_fn(
        "inventory-accumulator-currencies",
        InventoryAccumulator::currencies,
    );
    steel_engine.register_fn(
        "inventory-accumulator-main-currency",
        InventoryAccumulator::main_currency,
    );
    steel_engine.register_fn("inventory-accumulator-build", InventoryAccumulator::build);
}
