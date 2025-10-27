// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{hash_map::Entry, HashMap, HashSet};
use std::{fmt::Debug, hash::Hash, ops::Deref};

use super::{BookingError, Cost, Number, Position, Posting, Tolerance};

pub fn book<'p, 'i, P, T, I>(
    date: P::Date,
    postings: impl Iterator<Item = &'p P>,
    tolerance: T,
    inventory: I,
) -> Result<
    impl Iterator<
        Item = (
            P::Account,
            Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
        ),
    >,
    Vec<BookingError>,
>
where
    P: Posting + 'p + 'i,
    T: Tolerance,
    I: Fn(P::Account) -> Option<&'i Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>
        + Copy, // 'i for inventory
{
    let postings = postings.collect::<Vec<_>>();
    let mut updated_inventory =
        HashMap::<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>::default();

    let categorized = categorize_by_currency(&postings, inventory)?;

    Ok(updated_inventory.into_iter())
}
///
/// A list of positions for a currency satisfying these invariants:
/// 1. If there is a simple position without cost, it occurs first in the list
/// 2. All other positions are unique w.r.t cost.(currency, date, label)
/// 3. Sort order of these is by date then currency then label.
/// 4. All positions are non-empty.
#[derive(PartialEq, Eq, Default, Debug)]
struct CurrencyPositions<D, N, C, L>(Vec<CurrencyPosition<D, N, C, L>>);

impl<D, N, C, L> Deref for CurrencyPositions<D, N, C, L> {
    type Target = Vec<CurrencyPosition<D, N, C, L>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// CurrencyPosition for implicit currency, which is kept externally
struct CurrencyPosition<D, N, C, L> {
    units: N,
    cost: Option<Cost<D, N, C, L>>,
}

impl<D, N, C, L> CurrencyPosition<D, N, C, L> {
    fn is_below(&self, threshold: N) -> bool
    where
        N: Number + Ord,
    {
        // TODO ensure that costs are not left below threshold
        self.units.abs() <= threshold && self.cost.is_none()
    }
}

// See OG Beancount function of the same name
fn categorize_by_currency<'p, 'b, 'i, P, I>(
    postings: &'b [&'p P],
    inventory: I,
) -> Result<CategorizedPostings<'p, P, P::Currency>, Vec<BookingError>>
where
    P: Posting,
    I: Fn(P::Account) -> Option<&'i Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>
        + Copy, // 'i for inventory
    P::Date: 'i,
    P::Number: 'i,
    P::Currency: 'i,
    P::Label: 'i,
{
    let mut groups = CategorizedPostings::default();
    let mut auto_postings = Vec::default();
    let mut unknown = Vec::default();
    let mut account_currency_lookup = HashMap::<P::Account, Option<P::Currency>>::default();

    for (i_posting, posting) in postings.iter().enumerate() {
        let units_currency = posting.currency();
        let posting_cost_currency = posting.cost_currency();
        let posting_price_currency = posting.price_currency();
        let cost_currency = posting_cost_currency
            .as_ref()
            .cloned()
            .or(posting_price_currency.as_ref().cloned());
        let price_currency = posting_price_currency
            .as_ref()
            .cloned()
            .or(posting_cost_currency);

        let p = CurrenciedPosting {
            posting: *posting,
            units_currency,
            cost_currency,
            price_currency,
        };

        if posting.units().is_none() && posting.currency().is_none() {
            auto_postings.push(p);
        } else if let Some(bucket) = p.bucket() {
            groups.insert(bucket, p);
        } else {
            unknown.push((i_posting, p));
        }
    }

    // if we have a single unknown posting and all others are of the same currency,
    // infer that for the unknown
    if unknown.len() == 1 && groups.len() == 1 {
        let only_bucket = groups.keys().next().as_ref().cloned().unwrap().clone();
        let (i_u, u) = unknown.drain(..).next().unwrap();
        let inferred = CurrenciedPosting {
            posting: u.posting,
            units_currency: if u.price_currency.is_none() && u.cost_currency.is_none() {
                Some(only_bucket.clone())
            } else {
                None
            },
            cost_currency: u
                .cost_currency
                .as_ref()
                .cloned()
                .or(Some(only_bucket.clone())),
            price_currency: u.price_currency.or(Some(only_bucket.clone())),
        };
        groups.insert(only_bucket.clone(), inferred);
    }

    // infer all other unknown postings from account inference
    let mut errors = Vec::<BookingError>::default();
    for (i_u, u) in unknown {
        let u_account = u.posting.account();
        let inferred = CurrenciedPosting {
            posting: u.posting,
            units_currency: u.units_currency.or(account_currency(
                u_account,
                inventory,
                &mut account_currency_lookup,
            )),
            cost_currency: u.cost_currency, // TODO .or(account_currency_lookup.cost(u_account)),
            price_currency: u.price_currency,
        };
        if let Some(bucket) = inferred.bucket() {
            groups.insert(bucket, inferred);
        } else {
            errors.push(BookingError::Posting(
                i_u,
                crate::PostingBookingError::FailedToCategorize,
            ));
        }
    }

    Ok(groups)
}

// lookup account currency with memoization
fn account_currency<'i, A, D, N, C, L, I>(
    account: A,
    inventory: I,
    account_currency: &mut HashMap<A, Option<C>>,
) -> Option<C>
where
    A: Eq + Hash + Clone,
    D: 'i,
    N: 'i,
    C: Eq + Hash + Clone + 'i,
    L: 'i,
    I: Fn(A) -> Option<&'i Vec<Position<D, N, C, L>>> + Copy, // 'i for inventory
{
    account_currency.get(&account).cloned().unwrap_or_else(|| {
        let currency = if let Some(positions) = inventory(account.clone()) {
            let currencies = positions
                .iter()
                .map(|pos| pos.currency.clone())
                .collect::<HashSet<C>>();

            if currencies.len() == 1 {
                currencies.iter().next().cloned()
            } else {
                None
            }
        } else {
            None
        };

        account_currency.insert(account.clone(), currency.clone());

        currency
    })
}

#[derive(Debug)]
struct CategorizedPostings<'a, P, C>(HashMap<C, Vec<CurrenciedPosting<'a, P, C>>>);

// TODO why couldn't I derive this?
impl<'a, P, C> Default for CategorizedPostings<'a, P, C> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<'a, P, C> CategorizedPostings<'a, P, C> {
    fn insert(&mut self, bucket: C, p: CurrenciedPosting<'a, P, C>)
    where
        C: Eq + Hash,
    {
        use Entry::*;

        match self.0.entry(bucket) {
            Occupied(mut occupied) => {
                occupied.get_mut().push(p);
            }
            Vacant(vacant) => {
                vacant.insert(vec![p]);
            }
        }
    }
}

impl<'a, P, C> Deref for CategorizedPostings<'a, P, C> {
    type Target = HashMap<C, Vec<CurrenciedPosting<'a, P, C>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug)]
struct CurrenciedPosting<'a, P, C> {
    posting: &'a P,
    units_currency: Option<C>,
    cost_currency: Option<C>,
    price_currency: Option<C>,
}

impl<'a, P, C> CurrenciedPosting<'a, P, C> {
    fn bucket(&self) -> Option<C>
    where
        C: Clone,
    {
        self.cost_currency
            .as_ref()
            .cloned()
            .or(self.price_currency.as_ref().cloned())
            .or(self.units_currency.as_ref().cloned())
    }
}
