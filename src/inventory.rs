// TODO remove:
#![allow(dead_code, unused_variables)]
use joinery::JoinableIterator;
use std::{collections::HashMap, fmt::Display};
use steel::{
    gc::{MutContainer, Shared, SharedMut},
    rvals::{as_underlying_type, Custom, CustomType},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal,
};
use steel_derive::Steel;

use crate::{booking::*, types::*};

// TODO include commodities held at cost
#[derive(Clone, Steel, Default, Debug)]
pub(crate) struct InventoryBuilder(SharedMut<HashMap<String, PositionsBuilder>>); // indexed by currency

impl InventoryBuilder {
    fn with_initial_position(position: Position) -> InventoryBuilder {
        let mut positions = HashMap::default();
        positions.insert(position.units.currency.to_string(), position.into());

        Self(Shared::new(MutContainer::new(positions)))
    }

    // TODO this should include CostSpec and the booking method
    fn book(&mut self, position: Position) {
        // TODO cost
        let mut positions = self.0.write();
        match positions.get_mut(position.units.currency.as_str()) {
            Some(positions) => positions.book(position, Booking::Strict), // TODO booking method
            None => {
                positions.insert(position.units.currency.to_string(), position.into());
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Inventory {
    positions: Shared<Vec<Position>>, // of Position
}

impl Inventory {
    fn units(&self) -> HashMap<String, SteelDecimal> {
        self.iter_position_units().fold(
            HashMap::<String, SteelDecimal>::default(),
            |mut hm, units| {
                if hm.contains_key(&units.currency) {
                    *hm.get_mut(&units.currency).unwrap() += units.number.into();
                } else {
                    hm.insert(units.currency.clone(), units.number.into());
                }

                hm
            },
        )
    }

    fn positions(&self) -> Vec<Position> {
        (*self.positions).clone()
    }

    fn iter_position_units(&self) -> impl Iterator<Item = Amount> {
        self.positions.iter().map(|v| v.units.clone())
    }
}

impl From<&InventoryBuilder> for Option<Inventory> {
    fn from(value: &InventoryBuilder) -> Self {
        // return positions sorted by currency
        let InventoryBuilder(positions_by_currency) = value;
        let mut positions_by_currency = positions_by_currency.write();
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
                    .into_positions()
            })
            .collect::<Vec<_>>();

        if positions.is_empty() {
            None
        } else {
            Some(Inventory {
                positions: positions.into(),
            })
        }
    }
}

impl Display for Inventory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.positions.iter().join_with(", "))
    }
}

impl Custom for Inventory {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }

    fn equality_hint(&self, other: &dyn CustomType) -> bool {
        if let Some(other) = as_underlying_type::<Inventory>(other) {
            self == other
        } else {
            false
        }
    }
}

#[derive(Clone, Default, Debug)]
pub(crate) struct InventoriesBuilder {
    accounts: SharedMut<HashMap<String, InventoryBuilder>>, // indexed by account name
    currency_usage: SharedMut<HashMap<String, i32>>,
}

impl Custom for InventoriesBuilder {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", &self)))
    }
}

impl InventoriesBuilder {
    pub(crate) fn post(&mut self, posting: SteelVal) {
        if let SteelVal::Custom(posting) = posting {
            if let Some(posting) = as_underlying_type::<Posting>(posting.read().as_ref()) {
                let account_name = posting.account.as_str();
                let currency = posting.amount.currency.as_str();
                // TODO cost
                let position = Position {
                    units: posting.amount.clone(),
                    cost: None,
                };

                let mut accounts = self.accounts.write();
                match accounts.get_mut(account_name) {
                    Some(account) => account.book(position),
                    None => {
                        accounts.insert(
                            account_name.to_string(),
                            InventoryBuilder::with_initial_position(position),
                        );
                    }
                }
                let mut currency_usage = self.currency_usage.write();
                match currency_usage.get_mut(currency) {
                    Some(usage) => {
                        *usage += 1;
                    }
                    None => {
                        currency_usage.insert(currency.to_string(), 1);
                    }
                }
            } else {
                panic!("InventoryBuilder::post with something other than a Posting")
            }
        } else {
            panic!("InventoryBuilder::post with something other than a Custom")
        }
    }

    fn currencies(&self) -> Vec<String> {
        self.currency_usage
            .read()
            .keys()
            .map(|currency| currency.to_string())
            .collect::<Vec<String>>()
    }

    fn main_currency(&self) -> String {
        self.currency_usage
            .read()
            .iter()
            .max_by_key(|(_, n)| **n)
            .map(|(cur, _)| cur.clone())
            .unwrap_or(DEFAULT_CURRENCY.to_string())
    }

    // build inventories, filtering out empty ones
    fn build(self) -> HashMap<String, Inventory> {
        let Self { accounts, .. } = self;

        accounts
            .read()
            .iter()
            .filter_map(|(k, v)| Into::<Option<Inventory>>::into(v).map(|v| (k.clone(), v)))
            .collect::<HashMap<String, Inventory>>()
    }
}

const DEFAULT_CURRENCY: &str = "USD"; // ugh

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<InventoriesBuilder>("inventories-builder?");
    steel_engine.register_fn("inventories-builder", InventoriesBuilder::default);
    steel_engine.register_fn("inventories-builder-post", InventoriesBuilder::post);
    steel_engine.register_fn(
        "inventories-builder-currencies",
        InventoriesBuilder::currencies,
    );
    steel_engine.register_fn(
        "inventories-builder-main-currency",
        InventoriesBuilder::main_currency,
    );
    steel_engine.register_fn("inventories-builder-build", InventoriesBuilder::build);

    steel_engine.register_type::<Inventory>("inventory?");
    steel_engine.register_fn("inventory-positions", Inventory::positions);
    steel_engine.register_fn("inventory-units", Inventory::units);
}
