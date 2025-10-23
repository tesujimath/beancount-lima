// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser, Span, Spanned};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::{Deref, DerefMut},
};
use tabulator::{Align, Cell, Gap};
use time::Date;

use crate::config::LoaderConfig;
use crate::format::format;

#[derive(Debug)]
pub(crate) struct Loader<'a> {
    directives: Vec<Directive<'a>>,
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<&'a str, Span>,
    closed_accounts: hashbrown::HashMap<&'a str, Span>,
    accounts: HashMap<&'a str, AccountBuilder<'a>>,
    currency_usage: hashbrown::HashMap<&'a parser::Currency<'a>, i32>,
    config: LoaderConfig,
    default_booking: parser::Booking,
    inferred_tolerance: InferredTolerance<'a>,
    errors: Vec<parser::AnnotatedError>,
    warnings: Vec<parser::AnnotatedWarning>,
}

pub(crate) struct LoadSuccess<'a> {
    pub(crate) directives: Vec<Directive<'a>>,
    pub(crate) warnings: Vec<parser::AnnotatedWarning>,
}

pub(crate) struct LoadError {
    pub(crate) errors: Vec<parser::AnnotatedError>,
    pub(crate) warnings: Vec<parser::AnnotatedWarning>,
}

impl<'a> Loader<'a> {
    pub(crate) fn new(
        default_booking: parser::Booking,
        inferred_tolerance: InferredTolerance<'a>,
        config: LoaderConfig,
    ) -> Self {
        Self {
            directives: Vec::default(),
            open_accounts: hashbrown::HashMap::default(),
            closed_accounts: hashbrown::HashMap::default(),
            accounts: HashMap::default(),
            currency_usage: hashbrown::HashMap::default(),
            config,
            default_booking,
            inferred_tolerance,
            errors: Vec::default(),
            warnings: Vec::default(),
        }
    }

    // generate any errors before building
    fn validate(self) -> Result<LoadSuccess<'a>, LoadError> {
        let Self {
            directives,
            accounts,
            currency_usage,
            mut errors,
            warnings,
            ..
        } = self;

        // check for unused pad directives
        for account in accounts.values() {
            if let Some(pad_idx) = &account.pad_idx {
                errors.push(directives[*pad_idx].parsed.error("unused").into())
            }
        }

        if errors.is_empty() {
            Ok(LoadSuccess {
                directives,
                warnings,
            })
        } else {
            Err(LoadError { errors, warnings })
        }
    }

    pub(crate) fn collect<I>(mut self, directives: I) -> Result<LoadSuccess<'a>, LoadError>
    where
        I: IntoIterator<Item = &'a Spanned<parser::Directive<'a>>>,
    {
        for directive in directives {
            self.directive(directive);
        }

        self.validate()
    }

    fn directive(&mut self, directive: &'a Spanned<parser::Directive<'a>>) {
        use parser::DirectiveVariant::*;

        let date = *directive.date().item();
        let element = into_spanned_element(directive);

        // errors and warnings are accumulated within these methods
        if let Ok(loaded) = match directive.variant() {
            Transaction(transaction) => self.transaction(transaction, date, element),
            Price(price) => Ok(DirectiveVariant::NA),
            Balance(balance) => self.balance(balance, date, element),
            Open(open) => self.open(open, date, element),
            Close(close) => self.close(close, date, element),
            Commodity(commodity) => Ok(DirectiveVariant::NA),
            Pad(pad) => self.pad(pad, date, element),
            Document(document) => Ok(DirectiveVariant::NA),
            Note(note) => Ok(DirectiveVariant::NA),
            Event(event) => Ok(DirectiveVariant::NA),
            Query(query) => Ok(DirectiveVariant::NA),
        } {
            self.directives.push(Directive {
                parsed: directive,
                loaded,
            });
        }
    }

    fn transaction(
        &mut self,
        transaction: &'a parser::Transaction<'a>,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, ()> {
        match balance_transaction(transaction, &self.inferred_tolerance, &element) {
            Ok(weights) => {
                let mut postings = Vec::default();

                for (posting, weight) in transaction.postings().zip(weights) {
                    use WeightSource::*;
                    let (posting_amount, posting_currency) = match weight.source {
                        Native => (weight.number, weight.currency),
                        Cost(number, currency) => (number, currency),
                        Price(number, currency) => (number, currency),
                    };

                    let description = transaction.payee().map_or_else(
                        || {
                            transaction
                                .narration()
                                .map_or("post", |narration| narration.item())
                        },
                        |payee| payee.item(),
                    );

                    match self.post(
                        Some(posting),
                        into_spanned_element(posting),
                        date,
                        posting.account().item().as_ref(),
                        &weight,
                        posting.flag().map(|flag| *flag.item()),
                        posting.cost_spec().map(parser::Spanned::item),
                        description,
                    ) {
                        Ok(posting) => {
                            postings.push(posting);
                        }
                        Err(e) => {
                            self.errors.push(e);
                        }
                    }
                }

                Ok(DirectiveVariant::Transaction(Transaction { postings }))
            }
            Err(e) => {
                self.errors.push(e);

                Err(())
            }
        }
    }

    // not without guilt, but oh well
    #[allow(clippy::too_many_arguments)]
    // accumulate the post and return its alist attrs, or record error and return None
    fn post(
        &mut self,
        parsed: Option<&'a parser::Spanned<parser::Posting<'a>>>,
        element: parser::Spanned<Element>,
        date: Date,
        account_name: &'a str,
        weight: &Weight<'a>,
        flag: Option<parser::Flag>,
        cost_spec: Option<&'a parser::CostSpec>,
        description: &'a str,
    ) -> Result<Posting<'a>, parser::AnnotatedError> {
        tracing::debug!(
            "post {description} {:?} for {} with weight {:?}, cost_spec {:?}",
            parsed,
            account_name,
            weight,
            cost_spec
        );
        if self.open_accounts.contains_key(account_name) {
            let account = self.accounts.get_mut(account_name).unwrap();
            let amount = weight.amount_for_post();

            use hashbrown::hash_map::Entry::*;

            if account.is_currency_valid(amount.currency) {
                let booked =
                    match account.inventory.entry(amount.currency) {
                        Occupied(mut position) => {
                            let value = position.get_mut();
                            let booked =
                                value.book(date, amount.number, cost_spec, account.booking);

                            if value.is_empty() {
                                position.remove_entry();
                            }

                            booked
                        }

                        Vacant(position) => position
                            .insert(CurrencyPositionsBuilder::default())
                            .book(date, amount.number, cost_spec, account.booking),
                    }
                    .map_err(|message| element.error(message))?;

                // count currency usage
                match self.currency_usage.entry(amount.currency) {
                    Occupied(mut usage) => {
                        let usage = usage.get_mut();
                        *usage += 1;
                    }
                    Vacant(usage) => {
                        usage.insert(1);
                    }
                }

                // collate balance from inventory across all currencies, sorted so deterministic
                let mut currencies = account.inventory.keys().copied().collect::<Vec<_>>();
                currencies.sort();
                let balance = currencies
                    .into_iter()
                    .map(|cur| Amount {
                        number: account.inventory.get(cur).unwrap().units(),
                        currency: amount.currency,
                    })
                    .collect::<Vec<Amount>>();

                let posting = Posting {
                    parsed,
                    flag,
                    account: account_name,
                    amount: amount.number,
                    currency: amount.currency,
                    cost: None,  // TODO cost
                    price: None, // TODO price
                }; // ::new(account_name, amount.clone(), flag, cost);
                account.postings.push(posting.clone());

                account.balance_diagnostics.push(BalanceDiagnostic {
                    date,
                    description: Some(description),
                    amount: Some(amount.clone()),
                    balance,
                });

                Ok(posting)
            // TODO cost, price, flag, metadata
            } else {
                Err(element
                    .error_with_contexts(
                        "invalid currency for account",
                        vec![("open".to_string(), account.opened)],
                    )
                    .into())
            }
        } else if let Some(closed) = self.closed_accounts.get(account_name) {
            Err(element
                .error_with_contexts("account was closed", vec![("close".to_string(), *closed)])
                .into())
        } else {
            Err(element.error("account not open").into())
        }
    }

    // base account is known
    fn rollup_units(
        &self,
        base_account_name: &str,
    ) -> hashbrown::HashMap<&'a parser::Currency<'a>, Decimal> {
        if self.config.balance_rollup {
            let mut rollup_inventory =
                hashbrown::HashMap::<&'a parser::Currency<'a>, Decimal>::default();
            self.accounts
                .keys()
                .filter_map(|s| {
                    s.starts_with(base_account_name)
                        .then_some(&self.accounts.get(s).unwrap().inventory)
                })
                .for_each(|inv| {
                    inv.iter().for_each(|(cur, number)| {
                        use hashbrown::hash_map::Entry::*;
                        match rollup_inventory.entry(cur) {
                            Occupied(mut entry) => {
                                let existing_number = entry.get_mut();
                                *existing_number += number.units();
                            }
                            Vacant(entry) => {
                                entry.insert(number.units());
                            }
                        }
                    });
                });
            tracing::debug!(
                "rollup inventory for {:?} with config {:?} is {:?}",
                base_account_name,
                &rollup_inventory,
                &self.config
            );
            rollup_inventory
        } else {
            self.accounts
                .get(base_account_name)
                .unwrap()
                .inventory
                .iter()
                .map(|(cur, positions)| (*cur, positions.units()))
                .collect::<hashbrown::HashMap<_, _>>()
        }
    }

    fn balance(
        &mut self,
        balance: &'a parser::Balance,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, ()> {
        let account_name = balance.account().item().as_ref();
        let (margin, pad_idx) = if self.open_accounts.contains_key(&account_name) {
            // what's the gap between what we have and what the balance says we should have?
            let mut inventory_has_balance_currency = false;
            let mut margin = self
                .rollup_units(account_name)
                .into_iter()
                .map(|(cur, number)| {
                    if balance.atol().amount().currency().item() == cur {
                        inventory_has_balance_currency = true;
                        (
                            cur,
                            balance.atol().amount().number().item().value()
                                - Into::<Decimal>::into(number),
                        )
                    } else {
                        (cur, -(Into::<Decimal>::into(number)))
                    }
                })
                .filter_map(|(cur, number)| {
                    // discard anything below the tolerance
                    (number.abs()
                        > balance
                            .atol()
                            .tolerance()
                            .map(|x| *x.item())
                            .unwrap_or(Decimal::ZERO))
                    .then_some((cur, number))
                })
                .collect::<HashMap<_, _>>();

            let account = self.accounts.get_mut(&account_name).unwrap();

            // cope with the case of balance currency wasn't in inventory
            if !inventory_has_balance_currency
                && (balance.atol().amount().number().item().value().abs()
                    > balance
                        .atol()
                        .tolerance()
                        .map(|x| *x.item())
                        .unwrap_or(Decimal::ZERO))
            {
                margin.insert(
                    balance.atol().amount().currency().item(),
                    balance.atol().amount().number().item().value(),
                );
            }

            // pad can't last beyond balance
            (
                (!margin.is_empty()).then_some(margin),
                account.pad_idx.take(),
            )
        } else {
            self.errors
                .push(element.error("account not open".to_string()).into());
            (None, None)
        };

        match (margin, pad_idx) {
            (Some(margin), Some(pad_idx)) => {
                tracing::debug!(
                    "margin {}",
                    margin
                        .iter()
                        .map(|(cur, number)| format!("{} {}", -number, cur))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                let pad = self.directives[pad_idx].parsed;
                let pad_element = into_spanned_element(pad);
                let pad_date = *pad.date().item();
                let mut pad_postings = Vec::default();

                for (cur, number) in &margin {
                    let flag = Some(pad_flag());
                    tracing::debug!(
                        "back-filling pad at {} {} {} {}",
                        pad_idx,
                        &account_name,
                        number,
                        cur
                    );
                    let pad_weight = Weight {
                        number: *number,
                        currency: cur,
                        source: WeightSource::Native,
                    };

                    match self.post(
                        None,
                        pad_element.clone(),
                        pad_date,
                        account_name,
                        &pad_weight,
                        flag,
                        None,
                        "pad",
                    ) {
                        Ok(posting) => {
                            pad_postings.push(posting);
                        }
                        Err(e) => {
                            self.errors.push(e);
                        }
                    }
                }

                if let DirectiveVariant::Pad(pad) = &mut self.directives[pad_idx].loaded {
                    pad.postings = pad_postings;
                } else {
                    panic!(
                        "directive at {pad_idx} is not a pad, is {:?}",
                        &self.directives[pad_idx]
                    );
                }
            }
            (Some(margin), None) => {
                let account = self.accounts.get_mut(&account_name).unwrap();

                let reason = format!(
                    "accumulated {}, error {}",
                    if account.inventory.is_empty() {
                        "zero".to_string()
                    } else {
                        account.inventory.to_string()
                    },
                    margin
                        .iter()
                        .map(|(cur, number)| format!("{number} {cur}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                // determine context for error by collating postings since last balance
                let annotation = Cell::Column(
                    account
                        .balance_diagnostics
                        .drain(..)
                        .map(|bd| {
                            Cell::Row(
                                vec![
                                    (bd.date.to_string(), Align::Left).into(),
                                    bd.amount.map(|amt| amt.into()).unwrap_or_else(Cell::empty),
                                    Cell::Row(
                                        bd.balance
                                            .into_iter()
                                            .map(|amt| amt.into())
                                            .collect::<Vec<_>>(),
                                        Gap::Minor,
                                    ),
                                    bd.description
                                        .map(|d| (d, Align::Left).into())
                                        .unwrap_or_else(Cell::empty),
                                ],
                                Gap::Medium,
                            )
                        })
                        .collect::<Vec<_>>(),
                );

                self.errors.push(
                    element
                        .error(reason)
                        .with_annotation(annotation.to_string()),
                );

                tracing::debug!(
                    "adjusting inventory {:?} for {:?}",
                    &account.inventory,
                    &margin
                );

                // reset accumulated balance to what was asserted, to localise errors
                for (cur, units) in margin.into_iter() {
                    match account.inventory.entry(cur) {
                        hashbrown::hash_map::Entry::Occupied(mut entry) => {
                            let accumulated = entry.get_mut();
                            tracing::debug!(
                                "adjusting inventory {cur}.{:?} for {:?} by {}",
                                accumulated,
                                balance,
                                &units
                            );
                            accumulated.adjust_for_better_balance_violation_reporting(units);
                        }
                        hashbrown::hash_map::Entry::Vacant(entry) => {
                            tracing::debug!("adjusting empty inventory for balance to {}", &units);
                            entry
                                .insert(CurrencyPositionsBuilder::default())
                                .adjust_for_better_balance_violation_reporting(units);
                        }
                    };
                }
            }
            (None, Some(pad)) => {}
            (None, None) => {}
        }

        let account = self.accounts.get_mut(&account_name).unwrap();
        account.balance_diagnostics.clear();
        account.balance_diagnostics.push(BalanceDiagnostic {
            date,
            description: None,
            amount: None,
            balance: vec![balance.atol().amount().item().into()],
        });

        Ok(DirectiveVariant::NA)
    }

    fn open(
        &mut self,
        open: &'a parser::Open,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, ()> {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(open.account().item().as_ref()) {
            Occupied(open_entry) => {
                self.errors.push(
                    element
                        .error_with_contexts(
                            "account already opened",
                            vec![("open".to_string(), *open_entry.get())],
                        )
                        .into(),
                );
            }
            Vacant(open_entry) => {
                let span = element.span();
                open_entry.insert(*span);

                // cannot reopen a closed account
                if let Some(closed) = self.closed_accounts.get(&open.account().item().as_ref()) {
                    self.errors.push(
                        element
                            .error_with_contexts(
                                "account was closed",
                                vec![("close".to_string(), *closed)],
                            )
                            .into(),
                    );
                } else {
                    self.accounts.insert(
                        open.account().item().as_ref(),
                        AccountBuilder::with_currencies(
                            open.currencies().map(|c| c.item()),
                            open.booking()
                                .map(|booking| *booking.item())
                                .unwrap_or(self.default_booking),
                            *span,
                        ),
                    );
                }
            }
        }

        if let Some(booking) = open.booking() {
            if *booking.item() != parser::Booking::Strict {
                self.warnings.push(
                    element
                        .warning("booking methods other than strict are not yet supported")
                        .into(),
                );
            }
        }

        Ok(DirectiveVariant::NA)
    }

    fn close(
        &mut self,
        close: &'a parser::Close,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, ()> {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(close.account().item().as_ref()) {
            Occupied(open_entry) => {
                match self.closed_accounts.entry(close.account().item().as_ref()) {
                    Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        self.errors.push(
                            element
                                .error_with_contexts(
                                    "account was already closed",
                                    vec![("close".to_string(), *closed_entry.get())],
                                )
                                .into(),
                        );
                    }
                    Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(*element.span());
                    }
                }
            }
            Vacant(_) => {
                self.errors.push(element.error("account not open").into());
            }
        }

        Ok(DirectiveVariant::NA)
    }

    fn pad(
        &mut self,
        pad: &parser::Pad,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, ()> {
        let account_name = pad.account().item().as_ref();
        if self.open_accounts.contains_key(account_name) {
            let account = self.accounts.get_mut(account_name).unwrap();
            let source = pad.source().to_string();

            let unused_pad_idx = account.pad_idx.replace(self.directives.len());

            // unused pad directives are errors
            // https://beancount.github.io/docs/beancount_language_syntax.html#unused-pad-directives
            if let Some(unused_pad_idx) = unused_pad_idx {
                self.errors.push(
                    self.directives[unused_pad_idx]
                        .parsed
                        .error("unused")
                        .into(),
                );
            }

            Ok(DirectiveVariant::Pad(Pad {
                postings: Vec::default(),
            }))
        } else {
            self.errors.push(element.error("account not open").into());
            Err(())
        }
    }
}

#[derive(Default, Debug)]
struct Inventory<'a>(hashbrown::HashMap<&'a parser::Currency<'a>, CurrencyPositionsBuilder<'a>>); // only non-empty positions are maintained

impl<'a> Deref for Inventory<'a> {
    type Target = hashbrown::HashMap<&'a parser::Currency<'a>, CurrencyPositionsBuilder<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Inventory<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Display for Inventory<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // sort currencies for deterministic output
        let mut sorted_currency_positions =
            self.iter().map(|(cur, ps)| (*cur, ps)).collect::<Vec<_>>();
        sorted_currency_positions.sort_by_key(|(cur, ps)| *cur);
        format(
            f,
            &sorted_currency_positions,
            |f1, (cur, ps)| ps.format(f1, cur),
            ", ",
            None,
        )
    }
}

#[derive(Debug)]
struct AccountBuilder<'a> {
    // TODO support cost in inventory
    currencies: HashSet<&'a parser::Currency<'a>>,
    inventory: Inventory<'a>,
    opened: Span,
    // TODO booking
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    postings: Vec<Posting<'a>>,
    pad_idx: Option<usize>, // index in directives in Loader
    balance_diagnostics: Vec<BalanceDiagnostic<'a>>,
    booking: parser::Booking,
}

impl<'a> AccountBuilder<'a> {
    fn with_currencies<I>(currencies: I, booking: parser::Booking, opened: Span) -> Self
    where
        I: Iterator<Item = &'a parser::Currency<'a>>,
    {
        AccountBuilder {
            currencies: currencies.collect(),
            inventory: Inventory::default(),
            opened,
            postings: Vec::default(),
            pad_idx: None,
            balance_diagnostics: Vec::default(),
            booking,
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: &parser::Currency<'_>) -> bool {
        self.currencies.is_empty() || self.currencies.contains(currency)
    }
}

#[derive(Debug)]
struct BalanceDiagnostic<'a> {
    date: Date,
    description: Option<&'a str>,
    amount: Option<Amount<'a>>,
    balance: Vec<Amount<'a>>,
}

pub(crate) fn pad_flag() -> parser::Flag {
    parser::Flag::Letter(TryInto::<parser::FlagLetter>::try_into('P').unwrap())
}

mod balancing;
use balancing::{balance_transaction, Weight, WeightSource};

mod booking;
use booking::CurrencyPositionsBuilder;

mod types;
pub(crate) use types::*;
