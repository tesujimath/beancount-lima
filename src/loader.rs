// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{self as parser, Span, Spanned};
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use std::collections::{HashMap, HashSet};
use steel::{gc::Gc, rvals::SteelHashMap, SteelVal};
use tabulator::{Align, Cell, Gap};
use time::Date;

use crate::{bridge::convert_parser_options, config::LoaderConfig, prism::types as prism};

#[derive(Debug)]
pub(crate) struct Loader {
    directives: Vec<prism::Directive>,
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<String, Span>,
    closed_accounts: hashbrown::HashMap<String, Span>,
    accounts: HashMap<String, AccountBuilder>,
    currency_usage: hashbrown::HashMap<String, i32>,
    parser_options: SteelHashMap,
    config: LoaderConfig,
    errors: Vec<parser::AnnotatedError>,
    warnings: Vec<parser::AnnotatedWarning>,
}

pub(crate) struct LoadSuccess {
    pub(crate) directives: Vec<prism::Directive>,
    pub(crate) warnings: Vec<parser::AnnotatedWarning>,
}

pub(crate) struct LoadError {
    pub(crate) errors: Vec<parser::AnnotatedError>,
    pub(crate) warnings: Vec<parser::AnnotatedWarning>,
}

impl Loader {
    pub(crate) fn new(options: &parser::Options<'_>, config: LoaderConfig) -> Self {
        let parser_options = convert_parser_options(options)
            .map(|(k, v)| (SteelVal::SymbolV(k.to_string().into()), v))
            .collect::<steel::HashMap<SteelVal, SteelVal>>();
        let parser_options = Gc::new(parser_options).into();
        Self {
            directives: Vec::default(),
            open_accounts: hashbrown::HashMap::default(),
            closed_accounts: hashbrown::HashMap::default(),
            accounts: HashMap::default(),
            currency_usage: hashbrown::HashMap::default(),
            parser_options,
            config,
            errors: Vec::default(),
            warnings: Vec::default(),
        }
    }

    // generate any errors before building
    pub(crate) fn validate(self) -> Result<LoadSuccess, LoadError> {
        let Self {
            directives,
            accounts,
            currency_usage,
            parser_options,
            mut errors,
            warnings,
            ..
        } = self;

        // check for unused pad directives
        for account in accounts.values() {
            if let Some(pad_idx) = &account.pad_idx {
                errors.push(directives[*pad_idx].element.error("unused"))
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

    pub(crate) fn directive<'a>(
        &mut self,
        directive: &'a Spanned<parser::Directive<'a>>,
        inferred_tolerance: &InferredTolerance<'a>,
    ) {
        use parser::DirectiveVariant::*;

        let date = *directive.date().item();
        let element = Into::<prism::WrappedSpannedElement>::into(directive);

        match directive.variant() {
            Transaction(transaction) => {
                self.transaction(transaction, date, inferred_tolerance, element)
            }
            Price(price) => self.price(price, date, element),
            Balance(balance) => self.balance(balance, date, element),
            Open(open) => self.open(open, date, element),
            Close(close) => self.close(close, date, element),
            Commodity(commodity) => self.commodity(commodity, date, element),
            Pad(pad) => self.pad(pad, date, element),
            Document(document) => self.document(document, date, element),
            Note(note) => self.note(note, date, element),
            Event(event) => self.event(event, date, element),
            Query(query) => self.query(query, date, element),
        };
    }

    fn transaction<'a>(
        &mut self,
        transaction: &parser::Transaction,
        date: Date,
        inferred_tolerance: &InferredTolerance<'a>,
        element: prism::WrappedSpannedElement,
    ) {
        match balance_transaction(transaction, inferred_tolerance, &element) {
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

                    if let Some(posting) = self.post(
                        posting.into(),
                        date,
                        &posting.account().to_string(),
                        posting_amount,
                        posting_currency.to_string(),
                        posting.flag().map(|flag| flag.item().to_string()),
                        posting
                            .cost_spec()
                            .map(|cs| (date, cs.item(), &weight).into()),
                        description,
                    ) {
                        postings.push(posting);
                    }
                }

                let transaction = prism::Transaction {
                    postings: postings.into(),
                    flag: transaction.flag().to_string(),
                    payee: transaction.payee().map(|payee| payee.to_string()),
                    narration: transaction
                        .narration()
                        .map(|narration| narration.to_string()),
                };

                self.directives.push(prism::Directive {
                    date,
                    element,
                    variant: prism::DirectiveVariant::Transaction(transaction),
                })
            }
            Err(e) => {
                self.errors.push(e);
            }
        }
    }

    // not without guilt, but oh well
    #[allow(clippy::too_many_arguments)]
    // accumulate the post and return its alist attrs, or record error and return None
    fn post<D>(
        &mut self,
        element: prism::WrappedSpannedElement,
        date: Date,
        account_name: &str,
        amount: Decimal,
        currency: String,
        flag: Option<String>,
        cost: Option<prism::Cost>,
        description: D,
    ) -> Option<prism::Posting>
    where
        D: Into<String>,
    {
        if self.open_accounts.contains_key(account_name) {
            let account = self.accounts.get_mut(account_name).unwrap();

            use hashbrown::hash_map::Entry::*;

            if account.is_currency_valid(&currency) {
                match account.inventory.entry(currency.clone()) {
                    Occupied(mut position) => {
                        let value = position.get_mut();
                        *value += amount;

                        if value.is_zero() {
                            position.remove_entry();
                        }
                    }
                    Vacant(position) => {
                        position.insert(amount);
                    }
                }

                // count currency usage
                match self.currency_usage.entry(currency.clone()) {
                    Occupied(mut usage) => {
                        let usage = usage.get_mut();
                        *usage += 1;
                    }
                    Vacant(usage) => {
                        usage.insert(1);
                    }
                }

                let amount: prism::Amount = (amount, currency).into();

                // collate balance from inventory across all currencies, sorted so deterministic
                let mut currencies = account.inventory.keys().collect::<Vec<_>>();
                currencies.sort();
                let balance = currencies
                    .into_iter()
                    .map(|cur| {
                        let amount = account.inventory.get(cur).unwrap();
                        (*amount, cur.clone()).into()
                    })
                    .collect::<Vec<prism::Amount>>();

                let posting = prism::Posting::new(account_name, amount.clone(), flag, cost);
                account.postings.push(posting.clone());

                account.balance_diagnostics.push(BalanceDiagnostic {
                    date,
                    description: Some(description.into()),
                    amount: Some(amount.clone()),
                    balance,
                });

                Some(posting)
            // TODO cost, price, flag, metadata
            } else {
                self.errors.push(element.error_with_contexts(
                    "invalid currency for account",
                    vec![("open".to_string(), account.opened)],
                ));

                None
            }
        } else if let Some(closed) = self.closed_accounts.get(account_name) {
            self.errors.push(
                element.error_with_contexts(
                    "account was closed",
                    vec![("close".to_string(), *closed)],
                ),
            );

            None
        } else {
            self.errors.push(element.error("account not open"));

            None
        }
    }

    fn price(&mut self, price: &parser::Price, date: Date, element: prism::WrappedSpannedElement) {
        self.directives.push(prism::Directive {
            date,
            element,
            variant: prism::DirectiveVariant::Price(prism::Price {
                currency: price.currency().item().to_string(),
                amount: price.amount().item().into(),
            }),
        })
    }

    // base account is known
    fn rollup_inventory(&self, base_account_name: &str) -> hashbrown::HashMap<String, Decimal> {
        if self.config.balance_rollup {
            let mut rollup_inventory = hashbrown::HashMap::<String, Decimal>::default();
            self.accounts
                .keys()
                .filter_map(|s| {
                    s.starts_with(base_account_name)
                        .then_some(&self.accounts.get(s).unwrap().inventory)
                })
                .for_each(|inv| {
                    inv.iter().for_each(|(cur, number)| {
                        use hashbrown::hash_map::Entry::*;
                        match rollup_inventory.entry(cur.clone()) {
                            Occupied(mut entry) => {
                                let existing_number = entry.get_mut();
                                *existing_number += *number;
                            }
                            Vacant(entry) => {
                                entry.insert(*number);
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
                .clone()
        }
    }

    fn balance(
        &mut self,
        balance: &parser::Balance,
        date: Date,
        element: prism::WrappedSpannedElement,
    ) {
        let account_name = balance.account().item().to_string();
        let (margin, pad_idx) = if self.open_accounts.contains_key(&account_name) {
            // what's the gap between what we have and what the balance says we should have?
            let mut inventory_has_balance_currency = false;
            let mut margin = self
                .rollup_inventory(&account_name)
                .into_iter()
                .map(|(cur, number)| {
                    if balance.atol().amount().currency().item().as_ref() == cur.as_str() {
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
                    .then_some((cur.to_string(), number))
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
                    balance.atol().amount().currency().item().to_string(),
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
                .push(element.error("account not open".to_string()));
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

                let pad = &self.directives[pad_idx];
                let pad_element = pad.element.clone();
                let pad_date = pad.date;
                let mut pad_postings = Vec::default();

                for (cur, number) in &margin {
                    let pad_flag = Some(PAD_FLAG.to_string());
                    tracing::debug!(
                        "back-filling pad at {} {} {} {}",
                        pad_idx,
                        &account_name,
                        number,
                        cur
                    );
                    if let Some(posting) = self.post(
                        pad_element.clone(),
                        pad_date,
                        &account_name,
                        *number,
                        cur.clone(),
                        pad_flag,
                        None,
                        "pad",
                    ) {
                        pad_postings.push(posting);
                    }
                }

                if let prism::DirectiveVariant::Transaction(pad_transaction) =
                    &mut self.directives[pad_idx + 1].variant
                {
                    pad_transaction.postings = pad_postings.into();
                } else {
                    panic!("directive at {} is not a transaction", pad_idx + 1);
                }
            }
            (Some(margin), None) => {
                let account = self.accounts.get_mut(&account_name).unwrap();

                let reason = format!(
                    "accumulated {}, error {}",
                    if account.inventory.is_empty() {
                        "zero".to_string()
                    } else {
                        account
                            .inventory
                            .iter()
                            .map(|(cur, number)| format!("{number} {cur}"))
                            .collect::<Vec<String>>()
                            .join(", ")
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

                self.errors
                    .push(element.annotated_error(reason, annotation.to_string()));

                tracing::debug!(
                    "adjusting inventory {:?} for {:?}",
                    &account.inventory,
                    &margin
                );

                // reset accumulated balance to what was asserted, to localise errors
                for (k, v) in margin.into_iter() {
                    match account.inventory.entry(k) {
                        hashbrown::hash_map::Entry::Occupied(mut entry) => {
                            let accumulated = entry.get_mut();
                            tracing::debug!(
                                "adjusting inventory {} for balance by {}",
                                accumulated,
                                &v
                            );
                            *accumulated += v;
                        }
                        hashbrown::hash_map::Entry::Vacant(entry) => {
                            tracing::debug!("adjusting empty inventory for balance to {}", &v);
                            entry.insert(v);
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

        let balance = prism::Balance {
            account: account_name,
            amount: (
                balance.atol().amount().number().value(),
                balance.atol().amount().currency(),
            )
                .into(),
            tolerance: balance
                .atol()
                .tolerance()
                .map(|tolerance| (*tolerance.item())),
        };
        self.directives.push(prism::Directive {
            date,
            element,
            variant: prism::DirectiveVariant::Balance(balance),
        })
    }

    fn open(&mut self, open: &parser::Open, date: Date, element: prism::WrappedSpannedElement) {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(open.account().item().to_string()) {
            Occupied(open_entry) => {
                self.errors.push(element.error_with_contexts(
                    "account already opened",
                    vec![("open".to_string(), *open_entry.get())],
                ));
            }
            Vacant(open_entry) => {
                let span = element.span();
                open_entry.insert(span);

                // cannot reopen a closed account
                if let Some(closed) = self.closed_accounts.get(&open.account().item().to_string()) {
                    self.errors.push(element.error_with_contexts(
                        "account was closed",
                        vec![("close".to_string(), *closed)],
                    ));
                } else {
                    self.accounts.insert(
                        open.account().item().to_string(),
                        AccountBuilder::with_currencies(
                            open.currencies().map(|c| c.item().to_string()),
                            span,
                        ),
                    );
                }
            }
        }

        if let Some(booking) = open.booking() {
            if *booking.item() != parser::Booking::Strict {
                self.warnings.push(
                    element.warning("booking methods other than strict are not yet supported"),
                );
            }
        }

        let mut currencies = open
            .currencies()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        currencies.sort();

        self.directives.push(prism::Directive {
            date,
            element,
            variant: prism::DirectiveVariant::Open(prism::Open {
                account: open.account().item().to_string(),
                currencies,
                booking: open.booking().map(|booking| (*booking.item()).into()),
            }),
        })
    }

    fn close(&mut self, close: &parser::Close, date: Date, element: prism::WrappedSpannedElement) {
        use hashbrown::hash_map::Entry::*;
        match self.open_accounts.entry(close.account().item().to_string()) {
            Occupied(open_entry) => {
                match self
                    .closed_accounts
                    .entry(close.account().item().to_string())
                {
                    Occupied(closed_entry) => {
                        // cannot reclose a closed account
                        self.errors.push(element.error_with_contexts(
                            "account was already closed",
                            vec![("close".to_string(), *closed_entry.get())],
                        ));
                    }
                    Vacant(closed_entry) => {
                        open_entry.remove_entry();
                        closed_entry.insert(element.span());
                    }
                }
            }
            Vacant(_) => {
                self.errors.push(element.error("account not open"));
            }
        }

        self.directives.push(prism::Directive {
            date,
            element,
            variant: prism::DirectiveVariant::Close(prism::Close {
                account: close.account().item().to_string(),
            }),
        })
    }

    fn commodity(
        &mut self,
        commodity: &parser::Commodity,
        date: Date,
        element: prism::WrappedSpannedElement,
    ) {
        self.directives.push(prism::Directive {
            date,
            element,
            variant: prism::DirectiveVariant::Commodity(prism::Commodity {
                currency: commodity.currency().item().to_string(),
            }),
        })
    }

    fn pad(&mut self, pad: &parser::Pad, date: Date, element: prism::WrappedSpannedElement) {
        let account_name = pad.account().item().to_string();
        if self.open_accounts.contains_key(&account_name) {
            let account = self.accounts.get_mut(&account_name).unwrap();
            let source = pad.source().to_string();

            let unused_pad_idx = account.pad_idx.replace(self.directives.len());

            // unused pad directives are errors
            // https://beancount.github.io/docs/beancount_language_syntax.html#unused-pad-directives
            if let Some(unused_pad_idx) = unused_pad_idx {
                self.errors
                    .push(self.directives[unused_pad_idx].element.error("unused"));
            }

            self.directives.push(prism::Directive {
                date,
                element: element.clone(),
                variant: prism::DirectiveVariant::Pad(prism::Pad {
                    account: account_name,
                    source,
                }),
            });

            // also need a transaction to be back-filled later with whatever got padded
            self.directives.push(prism::Directive {
                date,
                element,
                variant: prism::DirectiveVariant::Transaction(prism::Transaction {
                    postings: Vec::default().into(),
                    flag: PAD_FLAG.into(),
                    payee: None,
                    narration: None,
                }),
            });
        } else {
            self.errors.push(element.error("account not open"));
        }
    }

    fn document(
        &mut self,
        document: &parser::Document,
        date: Date,
        element: prism::WrappedSpannedElement,
    ) {
        self.directives.push(prism::Directive {
            date,
            element: element.clone(),
            variant: prism::DirectiveVariant::Document(prism::Document {
                account: document.account().item().to_string(),
                path: document.path().item().to_string(),
            }),
        });
    }

    fn note(&mut self, note: &parser::Note, date: Date, element: prism::WrappedSpannedElement) {
        self.directives.push(prism::Directive {
            date,
            element: element.clone(),
            variant: prism::DirectiveVariant::Note(prism::Note {
                account: note.account().item().to_string(),
                comment: note.comment().item().to_string(),
            }),
        });
    }

    fn event(&mut self, event: &parser::Event, date: Date, element: prism::WrappedSpannedElement) {
        self.directives.push(prism::Directive {
            date,
            element: element.clone(),
            variant: prism::DirectiveVariant::Event(prism::Event {
                event_type: event.event_type().item().to_string(),
                description: event.description().item().to_string(),
            }),
        });
    }

    fn query(&mut self, query: &parser::Query, date: Date, element: prism::WrappedSpannedElement) {
        self.directives.push(prism::Directive {
            date,
            element: element.clone(),
            variant: prism::DirectiveVariant::Query(prism::Query {
                name: query.name().item().to_string(),
                content: query.content().item().to_string(),
            }),
        });
    }
}

#[derive(Debug)]
struct AccountBuilder {
    // TODO support cost in inventory
    currencies: HashSet<String>,
    inventory: hashbrown::HashMap<String, Decimal>, // only non-zero positions are maintained
    opened: Span,
    // TODO booking
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    postings: Vec<prism::Posting>,
    pad_idx: Option<usize>, // index in directives in Loader
    balance_diagnostics: Vec<BalanceDiagnostic>,
}

impl AccountBuilder {
    fn with_currencies<I>(currencies: I, opened: Span) -> Self
    where
        I: Iterator<Item = String>,
    {
        AccountBuilder {
            currencies: currencies.collect(),
            inventory: hashbrown::HashMap::default(),
            opened,
            postings: Vec::default(),
            pad_idx: None,
            balance_diagnostics: Vec::default(),
        }
    }

    /// all currencies are valid unless any were specified during open
    fn is_currency_valid(&self, currency: &String) -> bool {
        self.currencies.is_empty() || self.currencies.contains(currency)
    }
}

#[derive(Debug)]
struct BalanceDiagnostic {
    date: Date,
    description: Option<String>,
    amount: Option<prism::Amount>,
    balance: Vec<prism::Amount>,
}

const PAD_FLAG: &str = "'P";

pub(crate) mod balancing;
use balancing::{balance_transaction, InferredTolerance, WeightSource};

mod types;
use types as loader;
