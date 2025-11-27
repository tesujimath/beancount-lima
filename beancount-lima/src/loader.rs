// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use beancount_lima_booking::Bookings;
use beancount_parser_lima::{self as parser, Span, Spanned};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};
use tabulator::{Align, Cell};
use time::Date;

use crate::format::GUTTER_MEDIUM;
use crate::{config::LoaderConfig, format::GUTTER_MINOR};

#[derive(Debug)]
pub(crate) struct Loader<'a, T> {
    directives: Vec<Directive<'a>>,
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<&'a str, Span>,
    closed_accounts: hashbrown::HashMap<&'a str, Span>,
    accounts: HashMap<&'a str, AccountBuilder<'a>>,
    currency_usage: hashbrown::HashMap<parser::Currency<'a>, i32>,
    config: LoaderConfig,
    default_booking: parser::Booking,
    inferred_tolerance: InferredTolerance<'a>,
    tolerance: T,
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

impl<'a, T> Loader<'a, T> {
    pub(crate) fn new(
        default_booking: parser::Booking,
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
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
        for directive in directives {
            self.directive(directive);
        }

        self.validate()
    }

    fn directive(&mut self, directive: &'a Spanned<parser::Directive<'a>>)
    where
        T: beancount_lima_booking::Tolerance<Currency = parser::Currency<'a>, Number = Decimal>,
    {
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
            Custom(custom) => Ok(DirectiveVariant::NA),
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
    ) -> Result<DirectiveVariant<'a>, ()>
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
        match self.book(&element, date, &postings, description) {
            Ok(postings) => Ok(DirectiveVariant::Transaction(Transaction { postings })),
            Err(e) => {
                self.errors.push(e);
                Err(())
            }
        }
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
                    .into()
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

                for interpolated in &interpolated_postings {
                    self.tally_currency_usage(interpolated.currency);
                }

                // use hashbrown::hash_map::Entry::*;

                for (account_name, updated_positions) in updated_inventory {
                    let account = self.validate_account(element, account_name)?;

                    account.positions = updated_positions;

                    // account.post(posting.clone(), Some(amount.currency), None);

                    // TODO
                    // account.balance_diagnostics.push(BalanceDiagnostic {
                    //     date,
                    //     description: Some(description),
                    //     amount: Some(amount.clone()),
                    //     balance,
                    // });
                }

                // let postings = interpolated_postings
                //     .into_iter()
                //     .map(|interpolated| Posting {
                //         Some(parsed: interpolated.posting),
                //         flag: None, // TODO pass through flag
                //         account: interpolated.posting.account(),
                //         units: interpolated.units,
                //         currency: interpolated.currency,
                //     });

                let postings = interpolated_postings
                    .into_iter()
                    .enumerate()
                    .map(|(idx, interpolated)| {
                        let posting = &postings[idx];

                        Posting {
                            flag: None, // TODO carry through flag
                            account: posting.account(),
                            units: interpolated.units,
                            currency: interpolated.currency,
                        }
                    })
                    .collect::<Vec<_>>();
                Ok(postings)
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
                        Err(element
                            .error(format!("{} on posting {}", e.to_string(), idx))
                            .into())
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
        currency: &'a parser::Currency<'a>,
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
            let mut rollup_inventory =
                hashbrown::HashMap::<parser::Currency<'a>, Decimal>::default();
            self.accounts
                .keys()
                .filter_map(|s| {
                    s.starts_with(base_account_name)
                        .then_some(self.accounts.get(s).unwrap().positions.units())
                })
                .for_each(|account| {
                    account.into_iter().for_each(|(cur, number)| {
                        use hashbrown::hash_map::Entry::*;
                        match rollup_inventory.entry(*cur) {
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
                .positions
                .units()
                .iter()
                .map(|(cur, number)| (**cur, *number))
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
        let account_rollup = self.rollup_units(account_name);
        let account = self.accounts.get_mut(&account_name).unwrap();
        let (margin, pad_idx) = if self.open_accounts.contains_key(&account_name) {
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

                    // let pad_weight = Weight {
                    //     number: *number,
                    //     currency: cur,
                    //     source: WeightSource::Native,
                    // };

                    // TODO actually book something
                    // if let Err(e) = self.book(
                    //     &element,
                    //     date,
                    //     updated_inventory,
                    //     interpolated_postings.into_iter(),
                    //     description,
                    // ) {
                    //     self.errors.push(e);
                    //     Err(())
                    // } else {
                    //     // where does this come from?
                    //     todo!()
                    //     // Ok(DirectiveVariant::Transaction(Transaction { postings }))
                    // }

                    // TODO use book instead of post
                    // match self.post(
                    //     None,
                    //     pad_element.clone(),
                    //     pad_date,
                    //     account_name,
                    //     &pad_weight,
                    //     flag,
                    //     None,
                    //     "pad",
                    // ) {
                    //     Ok(posting) => {
                    //         pad_postings.push(posting);
                    //     }
                    //     Err(e) => {
                    //         self.errors.push(e);
                    //     }
                    // }
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
                        .drain(..)
                        .map(|bd| {
                            Cell::Row(
                                vec![
                                    (bd.date.to_string(), Align::Left).into(),
                                    bd.amount.map(|amt| amt.into()).unwrap_or(Cell::Empty),
                                    Cell::Row(
                                        bd.balance
                                            .into_iter()
                                            .map(|amt| amt.into())
                                            .collect::<Vec<_>>(),
                                        GUTTER_MINOR,
                                    ),
                                    bd.description
                                        .map(|d| (d, Align::Left).into())
                                        .unwrap_or(Cell::Empty),
                                ],
                                GUTTER_MEDIUM,
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
                    "adjusting positions {:?} for {:?}",
                    &account.positions,
                    &margin
                );

                // TODO for new booking approach
                // reset accumulated balance to what was asserted, to localise errors
                // for (cur, units) in margin.into_iter() {
                //     match account.inventory.entry(cur) {
                //         hashbrown::hash_map::Entry::Occupied(mut entry) => {
                //             let accumulated = entry.get_mut();
                //             tracing::debug!(
                //                 "adjusting inventory {cur}.{:?} for {:?} by {}",
                //                 accumulated,
                //                 balance,
                //                 &units
                //             );
                //             accumulated.adjust_for_better_balance_violation_reporting(units);
                //         }
                //         hashbrown::hash_map::Entry::Vacant(entry) => {
                //             tracing::debug!("adjusting empty inventory for balance to {}", &units);
                //             entry
                //                 .insert(CurrencyPositionsBuilder::default())
                //                 .adjust_for_better_balance_violation_reporting(units);
                //         }
                //     };
                // }
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
                        AccountBuilder::new(
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

#[derive(Debug)]
struct AccountBuilder<'a> {
    // TODO support cost in inventory
    allowed_currencies: HashSet<&'a parser::Currency<'a>>,
    // TODO replace all use of inventory with positions
    positions: beancount_lima_booking::Positions<Date, Decimal, parser::Currency<'a>, &'a str>,
    opened: Span,
    // TODO booking
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    pad_idx: Option<usize>, // index in directives in Loader
    balance_diagnostics: Vec<BalanceDiagnostic<'a>>,
    booking: parser::Booking,
}

impl<'a> AccountBuilder<'a> {
    fn new<I>(allowed_currencies: I, booking: parser::Booking, opened: Span) -> Self
    where
        I: Iterator<Item = &'a parser::Currency<'a>>,
    {
        AccountBuilder {
            allowed_currencies: allowed_currencies.collect(),
            positions: beancount_lima_booking::Positions::default(),
            opened,
            pad_idx: None,
            balance_diagnostics: Vec::default(),
            booking,
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: &parser::Currency<'_>) -> bool {
        self.allowed_currencies.is_empty() || self.allowed_currencies.contains(currency)
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

mod types;
pub(crate) use types::*;

mod util;
