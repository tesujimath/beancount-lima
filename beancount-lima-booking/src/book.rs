// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{HashMap, HashSet};
use std::{fmt::Debug, hash::Hash, iter::once};

use super::{
    AnnotatedPosting, BookedAtCostPosting, Booking, BookingError, CostedPosting, HashMapOfVec,
    Number, Position, Posting, PostingBookingError, Tolerance,
};

pub fn book<'p, 'i, P, T, I, M>(
    date: P::Date,
    postings: impl Iterator<Item = &'p P>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<
    impl Iterator<
        Item = (
            P::Account,
            Vec<Position<P::Date, P::Number, P::Currency, P::Label>>,
        ),
    >,
    BookingError,
>
where
    P: Posting + Debug + 'p + 'i,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'i Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>
        + Copy, // 'i for inventory
    M: Fn(P::Account) -> Booking + Copy, // 'i for inventory
{
    let postings = postings.collect::<Vec<_>>();
    let mut updated_inventory =
        HashMap::<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>::default();

    let currency_groups = categorize_by_currency(&postings, inventory)?;

    for (cur, annotated_postings) in currency_groups {
        let Reductions {
            updated_inventory: updated_inventory_for_cur,
            costed_postings,
        } = book_reductions(
            date,
            cur.clone(),
            annotated_postings,
            tolerance,
            |account| {
                updated_inventory
                    .get(&account)
                    .or_else(|| inventory(account.clone()))
            },
            method,
        )?;

        tracing::debug!("book reductions {:?} {:?}", &cur, updated_inventory_for_cur);
        for (account, positions) in updated_inventory_for_cur {
            updated_inventory.insert(account, positions);
        }

        // TODO weights/interpolation/other booking
        // interpolate_units
    }

    Ok(updated_inventory.into_iter())
}

struct Reductions<'p, P>
where
    P: Posting,
{
    updated_inventory:
        HashMap<P::Account, Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>,
    costed_postings: Vec<CostedPosting<'p, P, P::Number, P::Currency>>,
}

fn book_reductions<'p, 'i, 'b, P, T, I, M>(
    date: P::Date,
    currency: P::Currency,
    annotateds: Vec<AnnotatedPosting<'p, P, P::Currency>>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<Reductions<'p, P>, BookingError>
where
    P: Posting + Debug + 'p + 'i,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'i Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>
        + Copy, // 'i for inventory
    M: Fn(P::Account) -> Booking + Copy, // 'i for inventory
{
    use CostedPosting::*;

    let mut updated_inventory = HashMap::default();

    let costed_postings = annotateds
        .into_iter()
        .map(|annotated| {
            let account = annotated.posting.account();

            match (
                &annotated.cost_currency,
                annotated.posting.units(),
                updated_inventory
                    .get(&account)
                    .or_else(|| inventory(account.clone())),
            ) {
                (Some(posting_cost_currency), Some(posting_units), Some(previous_positions)) => {
                    // TODO booking methods other than strict
                    // let method = method(account.clone());
                    // we already warn about this, so we simply ignore it here
                    // if method != Booking::Strict {
                    //     Err(BookingError::Transaction(
                    //         TransactionBookingError::UnsupportedBookingMethod(
                    //             method,
                    //             account.to_string(),
                    //         ),
                    //     ))
                    // } else
                    if let Some(ann_sign) = posting_units.sign()
                        && previous_positions
                            .iter()
                            .filter(|pos| pos.currency == currency && pos.cost.is_some())
                            .any(|pos| {
                                pos.units
                                    .sign()
                                    .is_some_and(|pos_sign| pos_sign != ann_sign)
                            })
                    {
                        // we found a position with cost and sign opposite to ours, so we have a reduction

                        // find positions whose costs match what we have
                        let matched_positions = previous_positions
                            .iter()
                            .enumerate()
                            .filter_map(|(i, pos)| {
                                pos.cost.as_ref().and_then(|pos_cost| {
                                    annotated
                                        .posting
                                        .matches_cost(date, pos_cost)
                                        .then_some((i, pos))
                                })
                            })
                            .collect::<Vec<_>>();

                        tracing::debug!(
                            "book_reductions matched {:?} with {:?}",
                            &annotated,
                            &matched_positions
                        );

                        if matched_positions.is_empty() {
                            Ok(Unbooked(annotated))
                        } else if matched_positions.len() == 1 {
                            let (i_matched, matched_pos) =
                                matched_positions.into_iter().next().unwrap();
                            // Book 'em, Danno!
                            tracing::debug!(
                                "cost-matched unique position at {}: {:?}",
                                i_matched,
                                &matched_pos
                            );
                            let cost_currency = matched_pos.currency.clone();
                            let cost_units =
                                matched_pos.cost.as_ref().unwrap().per_unit * posting_units;

                            let updated_positions = previous_positions
                                .iter()
                                .enumerate()
                                .map(|(i, pos)| {
                                    if i == i_matched {
                                        pos.with_accumulated(posting_units)
                                    } else {
                                        pos.clone()
                                    }
                                })
                                .collect::<Vec<_>>();
                            tracing::debug!(
                                "updated positions for {:?}: {:?}",
                                &account,
                                &updated_positions
                            );
                            updated_inventory.insert(account.clone(), updated_positions);

                            Ok(Booked(BookedAtCostPosting {
                                posting: annotated.posting,
                                idx: annotated.idx,
                                units: cost_units,
                                currency: cost_currency,
                            }))
                        } else if tolerance.sum_is_tolerably_close_to_zero(
                            previous_positions
                                .iter()
                                .filter_map(|pos| pos.cost.is_some().then_some(pos.units))
                                .chain(once(posting_units)),
                            &currency,
                        ) {
                            // this is "sell everything", that is, existing positions at cost together with this one sum to zero-ish
                            // updated_inventory
                            let cost_currencies = matched_positions
                                .iter()
                                .map(|(_i, pos)| pos.cost.as_ref().unwrap().currency.clone())
                                .collect::<HashSet<_>>();

                            if cost_currencies.len() == 1 {
                                let cost_currency = cost_currencies.into_iter().next().unwrap();
                                let cost_units: P::Number = matched_positions
                                    .iter()
                                    .map(|(_i, pos)| {
                                        pos.cost.as_ref().unwrap().per_unit * posting_units
                                    })
                                    .sum();

                                let matched_positions = matched_positions
                                    .iter()
                                    .map(|(i, _)| *i)
                                    .collect::<HashSet<_>>();

                                let updated_positions = previous_positions
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, pos)| {
                                        matched_positions.contains(&i).then_some(pos.clone())
                                    })
                                    .collect::<Vec<_>>();
                                updated_inventory.insert(account.clone(), updated_positions);

                                Ok(Booked(BookedAtCostPosting {
                                    posting: annotated.posting,
                                    idx: annotated.idx,
                                    units: cost_units,
                                    currency: cost_currency,
                                }))
                            } else {
                                Err(BookingError::Posting(
                                    annotated.idx,
                                    PostingBookingError::MultipleCostCurrenciesMatch,
                                ))
                            }
                        } else {
                            Err(BookingError::Posting(
                                annotated.idx,
                                PostingBookingError::MultipleCostSpecMatches,
                            ))
                        }
                    } else {
                        Ok(Unbooked(annotated))
                    }
                }
                _ => Ok(Unbooked(annotated)),
            }
        })
        .collect::<Result<Vec<_>, BookingError>>()?;

    Ok(Reductions {
        updated_inventory,
        costed_postings,
    })
}

// See OG Beancount function of the same name
fn categorize_by_currency<'p, 'b, 'i, P, I>(
    postings: &'b [&'p P],
    inventory: I,
) -> Result<HashMapOfVec<P::Currency, AnnotatedPosting<'p, P, P::Currency>>, BookingError>
where
    P: Posting,
    I: Fn(P::Account) -> Option<&'i Vec<Position<P::Date, P::Number, P::Currency, P::Label>>>
        + Copy, // 'i for inventory
    P::Date: 'i,
    P::Number: 'i,
    P::Currency: 'i,
    P::Label: 'i,
{
    let mut currency_groups = HashMapOfVec::default();
    let mut auto_postings =
        HashMap::<Option<P::Currency>, AnnotatedPosting<'p, P, P::Currency>>::default();
    let mut unknown = Vec::default();
    let mut account_currency_lookup = HashMap::<P::Account, Option<P::Currency>>::default();

    for (idx, posting) in postings.iter().enumerate() {
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

        let p = AnnotatedPosting {
            posting: *posting,
            idx,
            units_currency,
            cost_currency,
            price_currency,
        };
        let bucket = p.bucket();

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
        let inferred = AnnotatedPosting {
            posting: u.posting,
            idx,
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
        currency_groups.push_or_insert(only_bucket.clone(), inferred);
    }

    // infer all other unknown postings from account inference
    for (idx, u) in unknown {
        let u_account = u.posting.account();
        let inferred = AnnotatedPosting {
            posting: u.posting,
            idx,
            units_currency: u.units_currency.or(account_currency(
                u_account,
                inventory,
                &mut account_currency_lookup,
            )),
            cost_currency: u.cost_currency, // TODO .or(account_currency_lookup.cost(u_account)),
            price_currency: u.price_currency,
        };
        if let Some(bucket) = inferred.bucket() {
            currency_groups.push_or_insert(bucket, inferred);
        } else {
            return Err(BookingError::Posting(
                idx,
                crate::PostingBookingError::FailedToCategorize,
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

        // add auto_posting to each currency group
        let all_buckets = currency_groups.keys().cloned().collect::<Vec<_>>();
        for bucket in all_buckets {
            currency_groups.push_or_insert(bucket, auto_posting.clone());
        }
    } else {
        for (bucket, auto_posting) in auto_postings.into_iter() {
            let bucket = bucket.unwrap();

            currency_groups.push_or_insert(bucket, auto_posting);
        }
    }

    Ok(currency_groups)
}

// lookup account currency with memoization
fn account_currency<'i, A, D, N, C, L, I>(
    account: A,
    inventory: I,
    account_currency: &mut HashMap<A, Option<C>>,
) -> Option<C>
where
    A: Eq + Hash + Clone,
    D: 'i + Copy,
    N: 'i + Copy,
    C: Eq + Hash + Clone + 'i,
    L: 'i + Clone,
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
