// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{HashMap, HashSet};
use std::{fmt::Debug, hash::Hash};

use super::{
    AnnotatedPosting, BookingError, CostSpec, HashMapOfVec, Number, Positions, PostingBookingError,
    PostingSpec, PriceSpec, TransactionBookingError,
};

// See OG Beancount function of the same name
pub(crate) fn categorize_by_currency<'a, 'b, P, I>(
    postings: &'b [P],
    inventory: I,
) -> Result<HashMapOfVec<P::Currency, AnnotatedPosting<P, P::Currency>>, BookingError>
where
    P: PostingSpec + Debug,
    I: Fn(P::Account) -> Option<&'a Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    P::Date: 'a,
    P::Number: 'a,
    P::Currency: 'a,
    P::Label: 'a,
{
    let mut currency_groups = HashMapOfVec::default();
    let mut auto_postings =
        HashMap::<Option<P::Currency>, AnnotatedPosting<P, P::Currency>>::default();
    let mut unknown = Vec::default();
    let mut account_currency_lookup = HashMap::<P::Account, Option<P::Currency>>::default();

    for (idx, posting) in postings.iter().enumerate() {
        let currency = posting.currency();
        let posting_cost_currency = posting.cost().and_then(|cost_spec| cost_spec.currency());
        let posting_price_currency = posting.price().and_then(|price_spec| price_spec.currency());
        let cost_currency = posting_cost_currency
            .as_ref()
            .cloned()
            .or(posting_price_currency.as_ref().cloned());
        let price_currency = posting_price_currency
            .as_ref()
            .cloned()
            .or(posting_cost_currency);

        let p = AnnotatedPosting {
            posting: posting.clone(),
            idx,
            currency,
            cost_currency,
            price_currency,
        };
        let bucket = p.bucket();
        tracing::debug!(
            "categorize_by_currency annotated {:?} with bucket {:?}",
            &p,
            &bucket
        );

        if posting.units().is_none() && posting.currency().is_none() {
            if auto_postings.contains_key(&bucket) {
                return Err(BookingError::Posting(
                    idx,
                    PostingBookingError::AmbiguousAutoPost,
                ));
            }
            auto_postings.insert(bucket, p);
        } else if let Some(bucket) = bucket {
            currency_groups.push_or_insert(bucket, p);
        } else {
            unknown.push((idx, p));
        }
    }

    tracing::debug!(
        "categorize_by_currency {} currency_groups {} unknowns: {:?}, {} auto_postings: {:?}",
        currency_groups.len(),
        unknown.len(),
        &unknown,
        auto_postings.len(),
        &auto_postings
    );

    // if we have a single unknown posting and all others are of the same currency,
    // infer that for the unknown
    if unknown.len() == 1 && currency_groups.len() == 1 {
        let only_bucket = currency_groups
            .keys()
            .next()
            .as_ref()
            .cloned()
            .unwrap()
            .clone();
        let (idx, u) = unknown.drain(..).next().unwrap();

        tracing::debug!("categorize_by_currency 1 unknown, 1 currency group");

        // infer any missing currency from bucket only if there's no cost or price
        let currency = u.currency.or(
            if u.posting.price().is_none() && u.posting.cost().is_none() {
                Some(only_bucket.clone())
            } else {
                None
            },
        );

        let inferred = AnnotatedPosting {
            posting: u.posting,
            idx,
            currency,
            cost_currency: u
                .cost_currency
                .as_ref()
                .cloned()
                .or(Some(only_bucket.clone())),
            price_currency: u.price_currency.or(Some(only_bucket.clone())),
        };
        currency_groups.push_or_insert(only_bucket.clone(), inferred);
    }

    // infer all other unknown postings from account inference
    for (idx, u) in unknown {
        let u_account = u.posting.account();
        if let Some(bucket) = account_currency(u_account, inventory, &mut account_currency_lookup) {
            currency_groups.push_or_insert(bucket, u);
        } else {
            return Err(BookingError::Posting(
                idx,
                crate::PostingBookingError::CannotInferAnything,
            ));
        }
    }

    if let Some(auto_posting) = auto_postings.remove(&None) {
        if !auto_postings.is_empty() {
            return Err(BookingError::Posting(
                auto_posting.idx,
                PostingBookingError::AmbiguousAutoPost,
            ));
        }

        // can only have a currency-ambiguous auto-post if there's a single bucket
        let all_buckets = currency_groups.keys().cloned().collect::<Vec<_>>();
        if all_buckets.is_empty() {
            return Err(BookingError::Transaction(
                TransactionBookingError::CannotDetermineCurrencyForBalancing,
            ));
        } else if all_buckets.len() == 1 {
            let sole_bucket = all_buckets.into_iter().next().unwrap();
            currency_groups.push_or_insert(sole_bucket, auto_posting);
        } else {
            return Err(BookingError::Transaction(
                TransactionBookingError::AutoPostMultipleBuckets(
                    all_buckets
                        .into_iter()
                        .map(|cur| cur.to_string())
                        .collect::<Vec<_>>(),
                ),
            ));
        }
    } else {
        for (bucket, auto_posting) in auto_postings.into_iter() {
            let bucket = bucket.unwrap();

            currency_groups.push_or_insert(bucket, auto_posting);
        }
    }

    tracing::debug!(
        "categorize_by_currency {} currency_groups: {:?}",
        currency_groups.len(),
        &currency_groups
    );

    Ok(currency_groups)
}

// lookup account currency with memoization
fn account_currency<'a, A, D, N, C, L, I>(
    account: A,
    inventory: I,
    account_currency: &mut HashMap<A, Option<C>>,
) -> Option<C>
where
    A: Eq + Hash + Clone,
    D: Eq + Ord + Copy + Debug + 'a,
    C: Eq + Hash + Ord + Clone + Debug + 'a,
    N: Number + Debug + 'a,
    L: Eq + Ord + Clone + Debug + 'a,
    I: Fn(A) -> Option<&'a Positions<D, N, C, L>> + Copy,
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
