// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Span, Spanned,
};
use color_eyre::eyre::{eyre, Result, WrapErr};
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    iter::once,
    path::Path,
};
use steel::{
    gc::Gc,
    rvals::{IntoSteelVal, SteelHashMap, SteelVector},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelVal, Vector,
};
use steel_derive::Steel;

use crate::{config::LedgerBuilderConfig, steel_date::SteelDate, types::*};

#[derive(Clone, Debug, Steel)]
pub(crate) struct Ledger {
    pub(crate) sources: CustomShared<BeancountSources>,
    pub(crate) directives: SteelVector,
    pub(crate) options: SteelHashMap,
}

impl Ledger {
    /// Empty ledger
    pub(crate) fn empty() -> Self {
        Ledger {
            sources: BeancountSources::from("").into(),
            directives: Gc::new(Vector::default()).into(),
            options: Gc::new(steel::HashMap::default()).into(),
        }
    }

    pub(crate) fn parse_from<W>(
        path: &Path,
        config: LedgerBuilderConfig,
        error_w: W,
    ) -> Result<Self>
    where
        W: Write + Copy,
    {
        let sources =
            BeancountSources::try_from(path).wrap_err(format!("failed to read {path:?}"))?;
        let parser = BeancountParser::new(&sources);

        let mut builder = match parser.parse() {
            Ok(ParseSuccess {
                directives,
                options,
                plugins: _,
                warnings,
            }) => {
                sources.write(error_w, warnings)?;
                let mut builder = LedgerBuilder::new(&options, config);

                for directive in directives {
                    builder.directive(&directive);
                }
                Ok(builder)
            }

            Err(ParseError { errors, warnings }) => {
                sources.write(error_w, errors)?;
                sources.write(error_w, warnings)?;
                Err(eyre! {"parse error"})
            }
        }?;

        drop(parser);

        builder.validate();

        builder.build(sources, error_w)
    }

    fn sources(&self) -> CustomShared<BeancountSources> {
        self.sources.clone()
    }

    fn directives(&self) -> SteelVal {
        SteelVal::VectorV(self.directives.clone())
    }

    fn options(&self) -> SteelVal {
        SteelVal::HashMapV(self.options.clone())
    }

    pub(crate) fn register(self, steel_engine: &mut Engine) {
        steel_engine
            .register_external_value("*sources*", self.sources())
            .unwrap(); // can't fail
        steel_engine
            .register_external_value("*directives*", self.directives())
            .unwrap(); // can't fail
        steel_engine
            .register_external_value("*options*", self.options())
            .unwrap(); // can't fail
    }
}

fn write_ffi_error(sources: &CustomShared<BeancountSources>, error: WrappedError) {
    tracing::debug!("write_ffi_error");

    sources
        .write(&std::io::stderr(), vec![error.as_ref().clone()])
        .unwrap();
}

fn write_ffi_errors(sources: &CustomShared<BeancountSources>, errors: Vec<WrappedError>) {
    tracing::debug!("write_ffi_errors");

    sources
        .write(
            &std::io::stderr(),
            errors
                .iter()
                .map(|e| e.as_ref().clone())
                .collect::<Vec<_>>(),
        )
        .unwrap();
}

#[derive(Debug)]
struct LedgerBuilder {
    directives: Vec<Directive>,
    // hashbrown HashMaps are used here for their Entry API, which is still unstable in std::collections::HashMap
    open_accounts: hashbrown::HashMap<String, Span>,
    closed_accounts: hashbrown::HashMap<String, Span>,
    accounts: HashMap<String, AccountBuilder>,
    currency_usage: hashbrown::HashMap<String, i32>,
    inferred_tolerance: InferredTolerance,
    parser_options: SteelHashMap,
    config: LedgerBuilderConfig,
    errors: Vec<parser::AnnotatedError>,
}

impl LedgerBuilder {
    fn new(options: &parser::Options<'_>, config: LedgerBuilderConfig) -> Self {
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
            inferred_tolerance: InferredTolerance::new(options),
            parser_options,
            config,
            errors: Vec::default(),
        }
    }

    // generate any errors before building
    fn validate(&mut self) {
        // check for unused pad directives
        for account in self.accounts.values() {
            if let Some(pad_idx) = &account.pad_idx {
                self.errors
                    .push(self.directives[*pad_idx].element.error("unused"))
            }
        }
    }

    fn build<W>(self, sources: BeancountSources, error_w: W) -> Result<Ledger>
    where
        W: Write + Copy,
    {
        let Self {
            directives,
            accounts,
            currency_usage,
            parser_options,
            errors,
            ..
        } = self;

        if errors.is_empty() {
            Ok(Ledger {
                sources: sources.into(),
                directives: Gc::new(
                    directives
                        .into_iter()
                        .map(|directive| directive.into_steelval().unwrap())
                        .collect::<Vector<SteelVal>>(),
                )
                .into(),
                options: parser_options,
            })
        } else {
            sources.write(error_w, errors)?;
            Err(eyre!("builder error"))
        }
    }

    fn directive(&mut self, directive: &Spanned<parser::Directive>) {
        use parser::DirectiveVariant::*;

        let date: SteelDate = (*directive.date().item()).into();
        let element = Into::<WrappedSpannedElement>::into(directive);

        match directive.variant() {
            Transaction(transaction) => self.transaction(transaction, date, element),
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

    fn transaction(
        &mut self,
        transaction: &parser::Transaction,
        date: SteelDate,
        element: WrappedSpannedElement,
    ) {
        let mut postings = Vec::default();

        // determine auto-posting for at most one unspecified account
        let mut residual = hashbrown::HashMap::<&parser::Currency<'_>, Decimal>::default();
        let mut unspecified: Vec<&Spanned<parser::Posting<'_>>> = Vec::default();

        for posting in transaction.postings() {
            use hashbrown::hash_map::Entry::*;

            if let Some((amount, currency)) = get_posting_amount_for_balancing_transaction(posting)
            {
                match residual.entry(currency) {
                    Occupied(mut residual_entry) => {
                        let accumulated = residual_entry.get() + amount;
                        if accumulated.is_zero() {
                            residual_entry.remove_entry();
                        } else {
                            residual_entry.insert(accumulated);
                        }
                    }
                    Vacant(residual_entry) => {
                        residual_entry.insert(amount);
                    }
                }
            } else {
                unspecified.push(posting);
            }
        }

        // record each posting for which we have both amount and currency
        let mut coarsest_scale_for_tolerance =
            hashbrown::HashMap::<&parser::Currency, u32>::default();

        for posting in transaction
            .postings()
            .filter(|posting| posting.amount().is_some() && posting.currency().is_some())
        {
            let amount = posting.amount().unwrap().item().value();
            let currency = posting.currency().unwrap().item();

            // track coarsest scale so far by currency
            use hashbrown::hash_map::Entry::*;
            match coarsest_scale_for_tolerance.entry(currency) {
                Occupied(mut entry) => {
                    let coarsest_so_far = entry.get_mut();
                    if amount.scale() < *coarsest_so_far {
                        *coarsest_so_far = amount.scale();
                    }
                }
                Vacant(entry) => {
                    entry.insert(amount.scale());
                }
            }

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
                amount,
                currency.to_string(),
                posting.flag().map(|flag| flag.item().to_string()),
                description,
            ) {
                postings.push(posting);
            }
        }

        // auto-post if required
        if let Some(unspecified) = unspecified.pop() {
            for (currency, number) in residual {
                let description = transaction.payee().map_or_else(
                    || {
                        transaction
                            .narration()
                            .map_or("post", |narration| narration.item())
                    },
                    |payee| payee.item(),
                );

                if let Some(posting) = self.post(
                    unspecified.into(),
                    date,
                    &unspecified.account().to_string(),
                    -number,
                    currency.to_string(),
                    unspecified.flag().map(|flag| flag.item().to_string()),
                    description,
                ) {
                    postings.push(posting);
                }
            }
        } else {
            let residual = self
                .inferred_tolerance
                .remove_tolerated_residuals(residual, &coarsest_scale_for_tolerance);

            if !residual.is_empty() {
                // check if within tolerance
                self.errors.push(element.error(format!(
                            "unbalanced transaction, residual {}",
                            residual
                                .iter()
                                .map(|(cur, number)| format!("{} {}", -number, cur))
                                .collect::<Vec<String>>()
                                .join(", ")
                        )));
            }
        }
        // any other unspecified postings are errors
        for unspecified in unspecified.iter() {
            self.errors.push(
                unspecified
                    .error("more than one posting without amount/currency")
                    .into(),
            );
        }

        let transaction = Transaction {
            postings: Gc::new(
                postings
                    .into_iter()
                    .map(|posting| posting.into_steelval().unwrap())
                    .collect::<Vector<SteelVal>>(),
            )
            .into(),
            flag: transaction.flag().to_string().into(),
            payee: transaction.payee().map(|payee| payee.to_string().into()),
            narration: transaction
                .narration()
                .map(|narration| narration.to_string().into()),
        };

        self.directives.push(Directive {
            date,
            element,
            variant: DirectiveVariant::Transaction(transaction),
        })
    }

    // not without guilt, but oh well
    #[allow(clippy::too_many_arguments)]
    // accumulate the post and return its alist attrs, or record error and return None
    fn post<D>(
        &mut self,
        element: WrappedSpannedElement,
        date: SteelDate,
        account_name: &str,
        amount: Decimal,
        currency: String,
        flag: Option<String>,
        description: D,
    ) -> Option<Posting>
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

                let amount: Amount = (amount, currency).into();

                // collate balance from inventory across all currencies, sorted so deterministic
                let mut currencies = account.inventory.keys().collect::<Vec<_>>();
                currencies.sort();
                let balance = currencies
                    .into_iter()
                    .map(|cur| {
                        let amount = account.inventory.get(cur).unwrap();
                        (*amount, cur.clone()).into()
                    })
                    .collect::<Vec<Amount>>();

                let posting = Posting::new(account_name, amount.clone(), flag);
                account.postings.push(posting.clone());

                account.balance_diagnostics.push(BalanceDiagnostic {
                    date,
                    description: Some(description.into()),
                    amount: Some(amount.clone()),
                    balance,
                });

                Some(posting)
            // TODO cost-or-costspec, price, flag, metadata
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

    fn price(&mut self, price: &parser::Price, date: SteelDate, element: WrappedSpannedElement) {}

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
        date: SteelDate,
        element: WrappedSpannedElement,
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
                        "pad",
                    ) {
                        pad_postings.push(posting);
                    }
                }

                if let DirectiveVariant::Transaction(pad_transaction) =
                    &mut self.directives[pad_idx + 1].variant
                {
                    pad_transaction.postings = Gc::new(
                        pad_postings
                            .into_iter()
                            .map(|posting| posting.into_steelval().unwrap())
                            .collect::<Vector<SteelVal>>(),
                    )
                    .into();
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
                let annotation = account
                    .balance_diagnostics
                    .drain(..)
                    .map(|bd| {
                        vec![
                            bd.date.to_string(),
                            bd.amount
                                .map(|amt| amt.to_string())
                                .unwrap_or("          ".to_string()),
                            bd.balance
                                .into_iter()
                                .map(|amt| amt.to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                            bd.description.unwrap_or("".to_string()),
                        ]
                    })
                    .collect::<Vec<_>>();

                use super::tabulate::Align::*;
                let annotation =
                    super::tabulate::tabulate(annotation, vec![Left, Right, Right, Left], "  ")
                        .join("\n");

                self.errors
                    .push(element.annotated_error(reason, annotation));

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

        let balance = Balance {
            account: account_name.into(),
            amount: (
                balance.atol().amount().number().value(),
                balance.atol().amount().currency(),
            )
                .into(),
            tolerance: balance
                .atol()
                .tolerance()
                .map(|tolerance| (*tolerance.item()).into()),
        };
        self.directives.push(Directive {
            date,
            element,
            variant: DirectiveVariant::Balance(balance),
        })
    }

    fn open(&mut self, open: &parser::Open, date: SteelDate, element: WrappedSpannedElement) {
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
    }

    fn close(&mut self, close: &parser::Close, date: SteelDate, element: WrappedSpannedElement) {
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
    }

    fn commodity(
        &mut self,
        commodity: &parser::Commodity,
        date: SteelDate,
        element: WrappedSpannedElement,
    ) {
    }

    fn pad(&mut self, pad: &parser::Pad, date: SteelDate, element: WrappedSpannedElement) {
        let account_name = pad.account().item().to_string();
        if self.open_accounts.contains_key(&account_name) {
            let account = self.accounts.get_mut(&account_name).unwrap();
            let source = pad.source().to_string().into();

            let unused_pad_idx = account.pad_idx.replace(self.directives.len());

            // unused pad directives are errors
            // https://beancount.github.io/docs/beancount_language_syntax.html#unused-pad-directives
            if let Some(unused_pad_idx) = unused_pad_idx {
                self.errors
                    .push(self.directives[unused_pad_idx].element.error("unused"));
            }

            self.directives.push(Directive {
                date,
                element: element.clone(),
                variant: DirectiveVariant::Pad(Pad { source }),
            });

            // also need a transaction to be back-filled later with whatever got padded
            self.directives.push(Directive {
                date,
                element,
                variant: DirectiveVariant::Transaction(Transaction {
                    postings: Gc::new(Vector::default()).into(),
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
        date: SteelDate,
        element: WrappedSpannedElement,
    ) {
    }

    fn note(&mut self, note: &parser::Note, date: SteelDate, element: WrappedSpannedElement) {}

    fn event(&mut self, event: &parser::Event, date: SteelDate, element: WrappedSpannedElement) {}

    fn query(&mut self, query: &parser::Query, date: SteelDate, element: WrappedSpannedElement) {}
}

#[derive(Debug)]
struct InferredTolerance {
    fallback: Option<Decimal>,
    by_currency: HashMap<String, Decimal>,

    multiplier: Decimal,
}

impl InferredTolerance {
    fn new(options: &parser::Options<'_>) -> Self {
        Self {
            fallback: options.inferred_tolerance_default_fallback(),
            by_currency: options
                .inferred_tolerance_defaults()
                .filter_map(|(cur, value)| cur.map(|cur| (cur.to_string(), value)))
                .collect::<HashMap<_, _>>(),
            multiplier: options.inferred_tolerance_multiplier(),
        }
    }

    // Beancount Precision & Tolerances, Martin Blais, May 2015, Updated May 2025
    // https://docs.google.com/document/d/1lgHxUUEY-UVEgoF6cupz2f_7v7vEF7fiJyiSlYYlhOo
    //
    // Note that inferring tolerances from cost is not currently supported.
    fn remove_tolerated_residuals<'a, 'b>(
        &self,
        residual: hashbrown::HashMap<&'a parser::Currency<'b>, Decimal>,
        coarsest_scale_for_tolerance: &hashbrown::HashMap<&parser::Currency, u32>,
    ) -> hashbrown::HashMap<&'a parser::Currency<'b>, Decimal> {
        residual
            .into_iter()
            .filter(|(cur, value)| {
                let abs_value = value.abs();

                let coarsest_scale = coarsest_scale_for_tolerance.get(cur);
                if coarsest_scale == Some(&0) {
                    // this particular residual is for an integer currency, so no tolerance
                    tracing::debug!("no tolerance for residual {:?} for integer currency {:?}", abs_value, cur);
                    true
                } else if let Some(tol) = self
                    .by_currency
                    .get(cur.as_ref())
                    .or(self.fallback.as_ref())
                {
                    let intolerable =  &abs_value > tol;

                    if intolerable {
                        tracing::debug!(
                            "tolerance {} for {:?} {:?} against {:?}",
                            if intolerable { "exceeded" } else { "ok" },
                            abs_value,
                            cur,
                            tol,
                        );
                    }

                    intolerable
                } else if let Some(coarsest_scale) = coarsest_scale {
                    let unit = Decimal::new(1, *coarsest_scale);
                    let tol = unit * self.multiplier;
                    let intolerable =  abs_value > tol;

                    if intolerable {
                        tracing::debug!(
                            "tolerance {} for {:?} {:?} against unit = {:?}, multiplier = {:?}, tol = {:?}",
                            if intolerable { "exceeded" } else { "ok" },
                            abs_value,
                            cur,
                            unit,
                            self.multiplier,
                            tol,
                        );
                    }

                    intolerable
                } else {
                    true
                }
            })
            .collect::<hashbrown::HashMap<_, _>>()
    }
}

/// For balancing a transaction we use the price if there is one, otherwise simply the amount.
///
/// I don't understand why you would have a price annotation not fully specified, so this case is ignored.
fn get_posting_amount_for_balancing_transaction<'a, 'b>(
    posting: &'b parser::Posting<'a>,
) -> Option<(Decimal, &'b parser::Currency<'a>)>
where
    'b: 'a,
{
    if let Some(price) = posting.price_annotation() {
        use parser::PriceSpec::*;
        use parser::ScopedExprValue::*;

        match price.item() {
            BareCurrency(_price_currency) => None,
            BareAmount(_price_amount) => None,
            CurrencyAmount(PerUnit(price_amount), price_currency) => posting
                .amount()
                .map(|amount| (amount.value() * price_amount.value(), price_currency)),
            CurrencyAmount(Total(price_amount), price_currency) => {
                Some((price_amount.value(), price_currency))
            }
        }
    } else if let (Some(amount), Some(currency)) = (posting.amount(), posting.currency()) {
        Some((amount.value(), currency.item()))
    } else {
        None
    }
}

#[derive(Debug)]
struct BalanceDiagnostic {
    date: SteelDate,
    description: Option<String>,
    amount: Option<Amount>,
    balance: Vec<Amount>,
}

#[derive(Debug)]
struct AccountBuilder {
    // TODO support cost in inventory
    currencies: HashSet<String>,
    inventory: hashbrown::HashMap<String, Decimal>, // only non-zero positions are maintained
    opened: Span,
    // TODO
    //  booking: Symbol, // defaulted correctly from options if omitted from Open directive
    postings: Vec<Posting>,
    pad_idx: Option<usize>, // index in directives in LedgerBuilder
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

/// Convert just those parser options that make sense to expose to Scheme.
/// TODO incomplete
fn convert_parser_options(
    options: &parser::Options<'_>,
) -> impl Iterator<Item = (&'static str, SteelVal)> {
    once((
        "name_assets",
        options
            .account_type_name(parser::AccountType::Assets)
            .to_string()
            .into_steelval()
            .unwrap(),
    ))
    .chain(once((
        "name_liabilities",
        options
            .account_type_name(parser::AccountType::Liabilities)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_equity",
        options
            .account_type_name(parser::AccountType::Equity)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_income",
        options
            .account_type_name(parser::AccountType::Income)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
    .chain(once((
        "name_expenses",
        options
            .account_type_name(parser::AccountType::Expenses)
            .to_string()
            .into_steelval()
            .unwrap(),
    )))
}

const PAD_FLAG: &str = "'P";

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_fn("sources-write-ffi-error", write_ffi_error);
    steel_engine.register_fn("sources-write-ffi-errors", write_ffi_errors);
}
