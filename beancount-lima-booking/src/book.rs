// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{hash_map::Entry, HashMap, HashSet};
use std::{cmp::Ordering, fmt::Debug, hash::Hash, iter::once};

use super::{
    interpolate_from_costed, AnnotatedPosting, BookedAtCostPosting, Booking, BookingError,
    CostImpl, CostSpec, CostedPosting, HashMapOfVec, InterpolatedCost, InterpolatedPosting,
    Interpolation, Number, Position, PostingBookingError, PostingSpec, PriceSpec, Tolerance,
    TransactionBookingError, UpdatedInventory,
};

pub fn book<'a, P, T, I, M>(
    date: P::Date,
    postings: impl Iterator<Item = P>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<UpdatedInventory<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(
            P::Account,
        ) -> Option<
            &'a Vec<
                Position<
                    CostImpl<P::Date, P::Number, P::Currency, P::Label>,
                    P::Date,
                    P::Number,
                    P::Currency,
                    P::Label,
                >,
            >,
        > + Copy, // 'i for inventory
    M: Fn(P::Account) -> Booking + Copy, // 'i for inventory
{
    let postings = postings.collect::<Vec<_>>();
    let mut updated_inventory = UpdatedInventory::default();

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

        let Interpolation { unbooked_postings } =
            interpolate_from_costed(&cur, costed_postings, tolerance)?;
        tracing::debug!("weights {:?}", &unbooked_postings);

        let updated_inventory_for_cur = book_augmentations(
            date,
            cur.clone(),
            unbooked_postings,
            tolerance,
            |account| {
                updated_inventory
                    .get(&account)
                    .or_else(|| inventory(account.clone()))
            },
            method,
        )?;

        tracing::debug!(
            "book augmentations {:?} {:?}",
            &cur,
            updated_inventory_for_cur
        );
        for (account, positions) in updated_inventory_for_cur {
            updated_inventory.insert(account, positions);
        }
    }

    Ok(updated_inventory)
}

#[derive(Debug)]
struct Reductions<P>
where
    P: PostingSpec,
{
    updated_inventory: UpdatedInventory<P>,
    costed_postings: Vec<CostedPosting<P, P::Number, P::Currency>>,
}

fn book_reductions<'a, 'b, P, T, I, M>(
    date: P::Date,
    currency: P::Currency,
    annotateds: Vec<AnnotatedPosting<P, P::Currency>>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<Reductions<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(
            P::Account,
        ) -> Option<
            &'a Vec<
                Position<
                    CostImpl<P::Date, P::Number, P::Currency, P::Label>,
                    P::Date,
                    P::Number,
                    P::Currency,
                    P::Label,
                >,
            >,
        > + Copy, // 'i for inventory
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
                                match (pos.cost.as_ref(), annotated.posting.cost().as_ref()) {
                                    (Some(pos_cost), Some(cost_spec)) => {
                                        cost_matches_spec(pos_cost, cost_spec, date)
                                            .then_some((i, pos))
                                    }
                                    _ => None,
                                }
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
                                cost_units,
                                cost_currency,
                            }))
                        } else if tolerance
                            .residual(
                                previous_positions
                                    .iter()
                                    .filter_map(|pos| pos.cost.is_some().then_some(pos.units))
                                    .chain(once(posting_units)),
                                &currency,
                            )
                            .is_none()
                        {
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
                                    cost_units,
                                    cost_currency,
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
        updated_inventory: updated_inventory.into(),
        costed_postings,
    })
}

// See OG Beancount function of the same name
fn categorize_by_currency<'a, 'b, P, I>(
    postings: &'b [P],
    inventory: I,
) -> Result<HashMapOfVec<P::Currency, AnnotatedPosting<P, P::Currency>>, BookingError>
where
    P: PostingSpec,
    I: Fn(
            P::Account,
        ) -> Option<
            &'a Vec<
                Position<
                    CostImpl<P::Date, P::Number, P::Currency, P::Label>,
                    P::Date,
                    P::Number,
                    P::Currency,
                    P::Label,
                >,
            >,
        > + Copy, // 'i for inventory
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
            currency: if u.price_currency.is_none() && u.cost_currency.is_none() {
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
            currency: u.currency.or(account_currency(
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
                crate::PostingBookingError::CannotCategorize,
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
                TransactionBookingError::AutoPostNoBuckets,
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

    Ok(currency_groups)
}

pub(crate) fn cost_matches_spec<D, N, C, L, CS>(
    cost: &CostImpl<D, N, C, L>,
    cost_spec: &CS,
    default_date: D,
) -> bool
where
    D: Eq + Copy,
    N: Eq + Copy,
    C: Eq + Clone,
    L: Eq + Clone,
    CS: CostSpec<Date = D, Number = N, Currency = C, Label = L>,
{
    !(
        cost_spec.date().unwrap_or(default_date) != cost.date
            || cost_spec
                .currency()
                .is_some_and(|cost_spec_currency| cost_spec_currency != cost.currency)
            || cost_spec
                .per_unit()
                .is_some_and(|cost_spec_units| cost_spec_units != cost.per_unit)
            || cost_spec
                .currency()
                .is_some_and(|cost_spec_currency| cost_spec_currency != cost.currency)
            || cost_spec.label().is_some_and(|cost_spec_label| {
                cost.label
                    .as_ref()
                    .is_some_and(|cost_label| *cost_label != cost_spec_label)
            })
        // TODO merge
    )
}

// lookup account currency with memoization
fn account_currency<'i, A, D, N, C, L, I>(
    account: A,
    inventory: I,
    account_currency: &mut HashMap<A, Option<C>>,
) -> Option<C>
where
    A: Eq + Hash + Clone,
    D: Eq + Ord + Copy + Debug + 'i,
    C: Eq + Hash + Ord + Clone + Debug + 'i,
    N: Number + Copy + Debug + 'i,
    L: Eq + Ord + Clone + Debug + 'i,
    I: Fn(A) -> Option<&'i Vec<Position<CostImpl<D, N, C, L>, D, N, C, L>>> + Copy, // 'i for inventory
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

fn book_augmentations<'a, 'b, P, T, I, M>(
    date: P::Date,
    currency: P::Currency,
    interpolateds: Vec<InterpolatedPosting<P, P::Number, P::Currency>>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<UpdatedInventory<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(
            P::Account,
        ) -> Option<
            &'a Vec<
                Position<
                    CostImpl<P::Date, P::Number, P::Currency, P::Label>,
                    P::Date,
                    P::Number,
                    P::Currency,
                    P::Label,
                >,
            >,
        > + Copy, // 'i for inventory
    M: Fn(P::Account) -> Booking + Copy, // 'i for inventory
{
    let mut updated_inventory = HashMap::default();

    for interpolated in interpolateds {
        use Entry::*;

        let posting = interpolated.posting;
        let account = posting.account();

        let previous_positions = match updated_inventory.entry(account.clone()) {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(inventory(account).cloned().unwrap_or_default()),
        };
        // .or_else(|| inventory(account.clone()));

        let posting_cost = posting.cost().map(|cost_spec| {
            // insist that cosy is fully specified
            let date = cost_spec.date().unwrap_or(date);
            let InterpolatedCost { per_unit, currency } = interpolated.cost.unwrap();
            let label = cost_spec.label();
            let merge = cost_spec.merge();

            CostImpl {
                date,
                per_unit,
                currency,
                label,
                merge,
            }
        });

        augment_positions(
            interpolated.units,
            currency.clone(),
            posting_cost,
            previous_positions,
        );
    }
    Ok(updated_inventory.into())
}

fn augment_positions<D, N, C, L>(
    units: N,
    currency: C,
    cost: Option<CostImpl<D, N, C, L>>,
    positions: &mut Vec<Position<CostImpl<D, N, C, L>, D, N, C, L>>,
) where
    D: Eq + Ord + Copy + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    N: Number + Copy + Debug,
    L: Eq + Ord + Clone + Debug,
{
    use Ordering::*;

    match positions.binary_search_by(|position| match (&cost, &position.cost) {
        (None, None) => Equal,
        (None, Some(_)) => Less,
        (Some(_), None) => Greater,
        (Some(cost0), Some(cost1)) => cost0.cmp(cost1),
    }) {
        Ok(i) => {
            let position = &mut positions[i];
            tracing::debug!(
                "augmenting position {:?} with {:?} {:?}",
                &position,
                units,
                &cost
            );
            position.units += units;
        }
        Err(i) => {
            let position = Position {
                units,
                currency,
                cost,
            };
            tracing::debug!("inserting new position {:?} at {i}", &position);
            positions.insert(i, position)
        }
    }
}
