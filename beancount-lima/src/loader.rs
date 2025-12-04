// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use beancount_lima_booking::{is_supported_method, Booking, Bookings, Interpolated};
use beancount_parser_lima::{self as parser, Span, Spanned};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
};
use tabulator::{Align, Cell};
use time::Date;

use crate::config::LoaderConfig;
use crate::format::GUTTER_MEDIUM;

#[derive(Debug)]
pub(crate) struct Loader<'a, T> {
    directives: Vec<Directive<'a>>,
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<&'a str, Span>,
    closed_accounts: hashbrown::HashMap<&'a str, Span>,
    accounts: HashMap<&'a str, AccountBuilder<'a>>,
    currency_usage: hashbrown::HashMap<parser::Currency<'a>, i32>,
    config: LoaderConfig,
    default_booking: Booking,
    inferred_tolerance: InferredTolerance<'a>,
    tolerance: T,
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

impl<'a, T> Loader<'a, T> {
    pub(crate) fn new(
        default_booking: Booking,
        inferred_tolerance: InferredTolerance<'a>,
        tolerance: T,
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
            tolerance,
            inferred_tolerance,
            warnings: Vec::default(),
        }
    }

    // generate any errors before building
    fn validate(
        self,
        mut errors: Vec<parser::AnnotatedError>,
    ) -> Result<LoadSuccess<'a>, LoadError> {
        let Self {
            directives,
            accounts,
            currency_usage,
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
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        let mut errors = Vec::default();

        for directive in directives {
            match self.directive(directive) {
                Ok(loaded) => {
                    self.directives.push(Directive {
                        parsed: directive,
                        loaded,
                    });
                }
                Err(e) => {
                    errors.push(e);
                }
            }
        }

        self.validate(errors)
    }

    fn directive(
        &mut self,
        directive: &'a Spanned<parser::Directive<'a>>,
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError>
    where
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        use parser::DirectiveVariant::*;

        let date = *directive.date().item();
        let element = into_spanned_element(directive);

        match directive.variant() {
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
            Custom(custom) => Ok(DirectiveVariant::NA),
        }
    }

    fn transaction(
        &mut self,
        transaction: &'a parser::Transaction<'a>,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError>
    where
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        let description = transaction.payee().map_or_else(
            || {
                transaction
                    .narration()
                    .map_or("post", |narration| narration.item())
            },
            |payee| payee.item(),
        );

        let postings = transaction.postings().collect::<Vec<_>>();
        self.book(&element, date, &postings, description)
            .map(|postings| DirectiveVariant::Transaction(Transaction { postings }))
    }

    fn book<P>(
        &mut self,
        element: &parser::Spanned<Element>,
        date: Date,
        postings: &[P],
        description: &'a str,
    ) -> Result<Vec<Posting<'a>>, parser::AnnotatedError>
    where
        P: beancount_lima_booking::PostingSpec<
                Date = time::Date,
                Account = &'a str,
                Currency = parser::Currency<'a>,
                Number = Decimal,
                CostSpec = &'a parser::CostSpec<'a>,
                PriceSpec = &'a parser::PriceSpec<'a>,
                Label = &'a str,
            > + Debug
            + 'a,
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        match beancount_lima_booking::book(
            date,
            postings,
            &self.tolerance,
            |accname| self.accounts.get(accname).map(|acc| &acc.positions),
            |accname| {
                self.accounts
                    .get(accname)
                    .map(|acc| acc.booking)
                    .unwrap_or(self.default_booking)
            },
        ) {
            Ok(Bookings {
                interpolated_postings,
                updated_inventory,
            }) => {
                tracing::debug!(
                    "booking {:?} {:?} for {:?}",
                    &interpolated_postings,
                    &updated_inventory,
                    element
                );

                // an interpolated posting arising from a reduction with multiple costs is mapped here to several postings,
                // each with a simple cost, so we don't have to deal with composite costs for a posting elsewhere
                let booked_postings = interpolated_postings
                    .into_iter()
                    .zip(postings)
                    .flat_map(|(interpolated, posting)| {
                        let Interpolated {
                            units,
                            currency,
                            cost,
                            price,
                            ..
                        } = interpolated;
                        if let Some(costs) = cost {
                            costs
                                .into_currency_costs()
                                .map(|(cur, cost)| Posting {
                                    flag: None, // TODO pass through posting flag
                                    account: posting.account(),
                                    units: cost.units,
                                    currency,
                                    cost: Some(cur_posting_cost_to_cost(cur, cost)),
                                    price: price.as_ref().cloned(),
                                })
                                .collect::<Vec<_>>()
                        } else {
                            vec![Posting {
                                flag: None, // TODO pass through posting flag
                                account: posting.account(),
                                units,
                                currency,
                                cost: None,
                                price,
                            }]
                        }
                    })
                    .collect::<Vec<_>>();

                // group postings by account and currency for balance diagnostics
                let mut account_posting_amounts =
                    hashbrown::HashMap::<&str, VecDeque<Amount<'_>>>::new();
                for booked in &booked_postings {
                    use hashbrown::hash_map::Entry::*;

                    let currency = booked.currency;
                    let units = booked.units;

                    self.tally_currency_usage(currency);

                    let account_name = booked.account;
                    let account = self.validate_account(element, account_name)?;

                    match account_posting_amounts.entry(account_name) {
                        Occupied(entry) => {
                            entry.into_mut().push_back((units, currency).into());
                        }
                        Vacant(entry) => {
                            let mut amounts = VecDeque::new();
                            amounts.push_back((units, currency).into());
                            entry.insert(amounts);
                        }
                    }
                }

                for (account_name, updated_positions) in updated_inventory {
                    let account = self.validate_account(element, account_name)?;

                    account.positions = updated_positions;

                    if let Some(mut posting_amounts) = account_posting_amounts.remove(account_name)
                    {
                        let last_amount = posting_amounts.pop_back().unwrap();

                        for amount in posting_amounts {
                            account.balance_diagnostics.push(BalanceDiagnostic {
                                date,
                                description: Some(description),
                                amount: Some(amount),
                                positions: None,
                            });
                        }

                        account.balance_diagnostics.push(BalanceDiagnostic {
                            date,
                            description: Some(description),
                            amount: Some(last_amount),
                            positions: Some(account.positions.clone()),
                        });
                    }
                }

                Ok(booked_postings)
            }
            Err(e) => {
                tracing::error!("booking error {}", &e);
                use beancount_lima_booking::BookingError::*;

                match &e {
                    Transaction(e) => Err(element.error(e.to_string()).into()),
                    Posting(idx, e) => {
                        // TODO attach posting error to actual posting
                        // let bad_posting = postings[*idx];
                        // bad_posting.error(e.to_string()).into()
                        Err(element.error(format!("{e} on posting {idx}")).into())
                    }
                }
            }
        }
    }

    fn validate_account(
        &mut self,
        element: &parser::Spanned<Element>,
        account_name: &'a str,
    ) -> Result<&mut AccountBuilder<'a>, parser::AnnotatedError> {
        if self.open_accounts.contains_key(account_name) {
            Ok(self.accounts.get_mut(account_name).unwrap())
        } else if let Some(closed) = self.closed_accounts.get(account_name) {
            Err(element
                .error_with_contexts("account was closed", vec![("close".to_string(), *closed)])
                .into())
        } else {
            Err(element.error("account not open").into())
        }
    }

    fn validate_account_and_currency(
        &mut self,
        element: &parser::Spanned<Element>,
        account_name: &'a str,
        currency: parser::Currency<'a>,
    ) -> Result<&mut AccountBuilder<'a>, parser::AnnotatedError> {
        let account = self.validate_account(element, account_name)?;

        if account.is_currency_valid(currency) {
            Ok(account)
        } else {
            Err(element
                .error_with_contexts(
                    "invalid currency for account",
                    vec![("open".to_string(), account.opened)],
                )
                .into())
        }
    }

    fn tally_currency_usage(&mut self, currency: parser::Currency<'a>) {
        use hashbrown::hash_map::Entry::*;

        match self.currency_usage.entry(currency) {
            Occupied(mut usage) => {
                let usage = usage.get_mut();
                *usage += 1;
            }
            Vacant(usage) => {
                usage.insert(1);
            }
        }
    }

    // base account is known
    fn rollup_units(
        &self,
        base_account_name: &str,
    ) -> hashbrown::HashMap<parser::Currency<'a>, Decimal> {
        if self.config.balance_rollup {
            let mut rollup_units = hashbrown::HashMap::<parser::Currency<'a>, Decimal>::default();
            self.accounts
                .keys()
                .filter_map(|s| {
                    s.starts_with(base_account_name)
                        .then_some(self.accounts.get(s).unwrap().positions.units())
                })
                .for_each(|account| {
                    account.into_iter().for_each(|(cur, number)| {
                        use hashbrown::hash_map::Entry::*;
                        match rollup_units.entry(*cur) {
                            Occupied(mut entry) => {
                                let existing_number = entry.get_mut();
                                *existing_number += number;
                            }
                            Vacant(entry) => {
                                entry.insert(number);
                            }
                        }
                    });
                });
            rollup_units
        } else {
            self.accounts
                .get(base_account_name)
                .map(|account| {
                    account
                        .positions
                        .units()
                        .iter()
                        .map(|(cur, number)| (**cur, *number))
                        .collect::<hashbrown::HashMap<_, _>>()
                })
                .unwrap_or_default()
        }
    }

    fn balance(
        &mut self,
        balance: &'a parser::Balance,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError>
    where
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        let account_name = balance.account().item().as_ref();
        let account_rollup = self.rollup_units(account_name);
        let account = self.validate_account(&element, account_name)?;

        let (margin, pad_idx) = {
            // what's the gap between what we have and what the balance says we should have?
            let mut inventory_has_balance_currency = false;
            let mut margin = account_rollup
                .into_iter()
                .map(|(cur, number)| {
                    if *balance.atol().amount().currency().item() == cur {
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
                    *balance.atol().amount().currency().item(),
                    balance.atol().amount().number().item().value(),
                );
            }

            // pad can't last beyond balance
            (
                (!margin.is_empty()).then_some(margin),
                account.pad_idx.take(),
            )
        };

        tracing::debug!("balance {:?} {:?}", &margin, pad_idx);

        match (margin, pad_idx) {
            (Some(margin), Some(pad_idx)) => {
                tracing::debug!(
                    "balance {:?} with margin {}",
                    balance,
                    margin
                        .iter()
                        .map(|(cur, number)| format!("{} {}", -number, cur))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                let pad = self.directives[pad_idx].parsed;
                let pad_element = into_spanned_element(pad);
                let pad_date = *pad.date().item();
                if let parser::DirectiveVariant::Pad(pad) = pad.variant() {
                    let pad_source = pad.source().item().as_ref();

                    let pad_postings = margin
                        .iter()
                        .flat_map(|(cur, number)| {
                            vec![
                                Posting {
                                    flag: Some(pad_flag()),
                                    account: balance.account().item().as_ref(),
                                    units: *number,
                                    currency: *cur,
                                    cost: None,
                                    price: None,
                                },
                                Posting {
                                    flag: Some(pad_flag()),
                                    account: pad_source,
                                    units: -*number,
                                    currency: *cur,
                                    cost: None,
                                    price: None,
                                },
                            ]
                        })
                        .collect::<Vec<_>>();

                    self.book(&element, date, &pad_postings, "pad")?;

                    if let DirectiveVariant::Pad(pad) = &mut self.directives[pad_idx].loaded {
                        pad.postings = pad_postings;
                        tracing::debug!("pad postings inserted for {:?}", pad);
                    }
                } else {
                    panic!(
                        "directive at {pad_idx} is not a pad, is {:?}",
                        &self.directives[pad_idx]
                    );
                }
            }
            (Some(margin), None) => {
                let reason = format!(
                    "accumulated {}, error {}",
                    if account.positions.is_empty() {
                        "zero".to_string()
                    } else {
                        account.positions.to_string()
                    },
                    margin
                        .iter()
                        .map(|(cur, number)| format!("{number} {cur}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                // determine context for error by collating postings since last balance
                let annotation = Cell::Stack(
                    account
                        .balance_diagnostics
                        .iter()
                        .map(|bd| {
                            Cell::Row(
                                vec![
                                    (bd.date.to_string(), Align::Left).into(),
                                    bd.amount
                                        .as_ref()
                                        .map(|amt| amt.into())
                                        .unwrap_or(Cell::Empty),
                                    bd.positions
                                        .as_ref()
                                        .map(positions_to_cell)
                                        .unwrap_or(Cell::Empty),
                                    bd.description
                                        .map(|d| (d, Align::Left).into())
                                        .unwrap_or(Cell::Empty),
                                ],
                                GUTTER_MEDIUM,
                            )
                        })
                        .collect::<Vec<_>>(),
                );

                let err = Err(element
                    .error(reason)
                    .with_annotation(annotation.to_string()));

                // reset accumulated balance to what was asserted, to localise errors
                for (cur, units) in margin.into_iter() {
                    account
                        .positions
                        .accumulate(units, cur, None, Booking::default());
                    // booking method doesn't matter if no cost
                }

                return err;
            }
            (None, Some(pad)) => {}
            (None, None) => {}
        }

        let account = self.accounts.get_mut(&account_name).unwrap();
        account.balance_diagnostics.clear();
        let balance_amount = balance.atol().amount().item();
        let balance_units = balance_amount.number().item().value();
        let balance_cur = *balance_amount.currency().item();
        let mut positions = Positions::default();
        positions.accumulate(balance_units, balance_cur, None, Booking::default());
        account.balance_diagnostics.push(BalanceDiagnostic {
            date,
            description: None,
            amount: None,
            positions: Some(positions),
        });

        Ok(DirectiveVariant::NA)
    }

    fn open(
        &mut self,
        open: &'a parser::Open,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError> {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(open.account().item().as_ref()) {
            Occupied(open_entry) => {
                return Err(element
                    .error_with_contexts(
                        "account already opened",
                        vec![("open".to_string(), *open_entry.get())],
                    )
                    .into());
            }
            Vacant(open_entry) => {
                let span = element.span();
                open_entry.insert(*span);

                // cannot reopen a closed account
                if let Some(closed) = self.closed_accounts.get(&open.account().item().as_ref()) {
                    return Err(element
                        .error_with_contexts(
                            "account was closed",
                            vec![("close".to_string(), *closed)],
                        )
                        .into());
                } else {
                    let mut booking = open
                        .booking()
                        .map(|booking| Into::<Booking>::into(*booking.item()))
                        .unwrap_or(self.default_booking);

                    if !is_supported_method(booking) {
                        let default_booking = Booking::default();
                        self.warnings.push(
                            element .warning(format!( "booking method {booking} unsupported, falling back to default {default_booking}" )) .into(),
                        );
                        booking = default_booking;
                    }

                    self.accounts.insert(
                        open.account().item().as_ref(),
                        AccountBuilder::new(open.currencies().map(|c| *c.item()), booking, *span),
                    );
                }
            }
        }

        if let Some(booking) = open.booking() {
            let booking = Into::<Booking>::into(*booking.item());
            if is_supported_method(booking) {
            } else {
                self.warnings.push(
                    element
                        .warning("booking method {} unsupported, falling back to default")
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
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError> {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(close.account().item().as_ref()) {
            Occupied(open_entry) => {
                match self.closed_accounts.entry(close.account().item().as_ref()) {
                    Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        return Err(element
                            .error_with_contexts(
                                "account was already closed",
                                vec![("close".to_string(), *closed_entry.get())],
                            )
                            .into());
                    }
                    Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(*element.span());
                    }
                }
            }
            Vacant(_) => {
                return Err(element.error("account not open").into());
            }
        }

        Ok(DirectiveVariant::NA)
    }

    fn pad(
        &mut self,
        pad: &'a parser::Pad<'a>,
        date: Date,
        element: parser::Spanned<Element>,
    ) -> Result<DirectiveVariant<'a>, parser::AnnotatedError> {
        let n_directives = self.directives.len();
        let account_name = pad.account().item().as_ref();
        let account = self.validate_account(&element, account_name)?;
        let source = pad.source().to_string();

        let unused_pad_idx = account.pad_idx.replace(n_directives);

        // unused pad directives are errors
        // https://beancount.github.io/docs/beancount_language_syntax.html#unused-pad-directives
        if let Some(unused_pad_idx) = unused_pad_idx {
            return Err(self.directives[unused_pad_idx]
                .parsed
                .error("unused")
                .into());
        }

        Ok(DirectiveVariant::Pad(Pad {
            postings: Vec::default(),
        }))
    }
}

#[derive(Debug)]
struct AccountBuilder<'a> {
    allowed_currencies: HashSet<parser::Currency<'a>>,
    positions: Positions<'a>,
    opened: Span,
    pad_idx: Option<usize>, // index in directives in Loader
    balance_diagnostics: Vec<BalanceDiagnostic<'a>>,
    booking: Booking,
}

impl<'a> AccountBuilder<'a> {
    fn new<I>(allowed_currencies: I, booking: Booking, opened: Span) -> Self
    where
        I: Iterator<Item = parser::Currency<'a>>,
    {
        AccountBuilder {
            allowed_currencies: allowed_currencies.collect(),
            positions: Positions::default(),
            opened,
            pad_idx: None,
            balance_diagnostics: Vec::default(),
            booking,
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: parser::Currency<'_>) -> bool {
        self.allowed_currencies.is_empty() || self.allowed_currencies.contains(&currency)
    }
}

#[derive(Debug)]
struct BalanceDiagnostic<'a> {
    date: Date,
    description: Option<&'a str>,
    amount: Option<Amount<'a>>,
    positions: Option<Positions<'a>>,
}

pub(crate) fn pad_flag() -> parser::Flag {
    parser::Flag::Letter(TryInto::<parser::FlagLetter>::try_into('P').unwrap())
}

mod types;
pub(crate) use types::*;

mod util;
