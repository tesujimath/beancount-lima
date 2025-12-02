// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

// Since 0.14.0 beancount-parser-lima no longer provides default options

use beancount_parser_lima as parser;
use rust_decimal::Decimal;

pub(crate) fn default_title() -> &'static str {
    "Beancount"
}

pub(crate) fn default_account_previous_balances() -> parser::Subaccount<'static> {
    "Opening-Balances".try_into().unwrap()
}

pub(crate) fn default_account_previous_earnings() -> parser::Subaccount<'static> {
    "Earnings:Previous".try_into().unwrap()
}

pub(crate) fn default_account_previous_conversions() -> parser::Subaccount<'static> {
    "Conversions:Previous".try_into().unwrap()
}

pub(crate) fn default_account_current_earnings() -> parser::Subaccount<'static> {
    "Earnings:Current".try_into().unwrap()
}

pub(crate) fn default_account_current_conversions() -> parser::Subaccount<'static> {
    "Conversions:Current".try_into().unwrap()
}

pub(crate) fn default_account_unrealized_gains() -> parser::Subaccount<'static> {
    "Earnings:Unrealized".try_into().unwrap()
}

pub(crate) fn default_conversion_currency() -> parser::Currency<'static> {
    parser::Currency::try_from("NOTHING").unwrap()
}

pub(crate) fn default_inferred_tolerance_multiplier() -> Decimal {
    Decimal::new(5, 1) // 0.5
}

pub(crate) fn default_infer_tolerance_from_cost() -> bool {
    false
}

pub(crate) fn default_render_commas() -> bool {
    false
}

pub(crate) fn default_plugin_processing_mode() -> parser::PluginProcessingMode {
    parser::PluginProcessingMode::Default
}
