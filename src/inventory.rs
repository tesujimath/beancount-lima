// TODO remove:
#![allow(dead_code, unused_variables)]
use rust_decimal::Decimal;
use std::collections::HashMap;
use steel::{
    gc::Gc,
    rvals::{as_underlying_type, Custom, IntoSteelVal, SteelString},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal,
};
use steel_derive::Steel;

use crate::{steel_decimal::SteelDecimal, types::*};

// TODO include commodities held at cost
#[derive(Clone, Steel, Default, Debug)]
pub(crate) struct InventoryBuilder {
    positions: HashMap<String, Decimal>, // indexed by currency
}

impl InventoryBuilder {
    fn with_initial_balance(units: Decimal, currency: &str) -> InventoryBuilder {
        let mut positions = HashMap::default();
        positions.insert(currency.to_string(), units);

        Self { positions }
    }

    fn post(&mut self, units: Decimal, currency: &str) {
        match self.positions.get_mut(currency) {
            Some(bal) => *bal += units,
            None => {
                self.positions.insert(currency.to_string(), units);
            }
        }
    }

    // build all positions, discarding those with zero balance
    fn build(self) -> SteelVal {
        let Self { positions } = self;

        SteelVal::HashMapV(
            Gc::new(
                positions
                    .into_iter()
                    .filter_map(|(k, v)| {
                        (!v.is_zero()).then_some((
                            k.into(),
                            Into::<SteelDecimal>::into(v).into_steelval().unwrap(),
                        ))
                    })
                    .collect::<steel::HashMap<SteelVal, SteelVal>>(),
            )
            .into(),
        )
    }
}

#[derive(Clone, Default, Debug)]
pub(crate) struct InventoryAccumulator {
    accounts: HashMap<String, InventoryBuilder>, // indexed by account name
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
            if let Some(posting) = as_underlying_type::<Posting>(posting.read().as_ref()) {
                let account_name = posting.account.as_str();
                let units = posting.amount.number.into();
                let currency = posting.amount.currency.as_str();

                match self.accounts.get_mut(account_name) {
                    Some(account) => account.post(units, currency),
                    None => {
                        self.accounts.insert(
                            account_name.to_string(),
                            InventoryBuilder::with_initial_balance(units, currency),
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
                    .map(|(k, v)| (k.into(), v.build()))
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
