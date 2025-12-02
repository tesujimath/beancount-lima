// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::{HashMap, HashSet};
use std::{fmt::Debug, hash::Hash, iter::once};

use super::{
    AnnotatedPosting, BookedOrUnbookedPosting, Booking, BookingError, Cost, CostSpec, Interpolated,
    Inventory, Number, Position, Positions, PostingBookingError, PostingCost, PostingCosts,
    PostingSpec, Tolerance,
};

#[derive(Debug)]
pub(crate) struct Reductions<P>
where
    P: PostingSpec,
{
    pub(crate) updated_inventory: Inventory<P::Account, P::Date, P::Number, P::Currency, P::Label>,
    pub(crate) postings: Vec<BookedOrUnbookedPosting<P>>,
}

pub(crate) fn book_reductions<'a, P, T, I, M>(
    date: P::Date,
    annotateds: Vec<AnnotatedPosting<P, P::Currency>>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<Reductions<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'a Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    M: Fn(P::Account) -> Booking + Copy,
{
    let mut updated_inventory = HashMap::default();
    let mut costed_postings = Vec::default();

    tracing::debug!(
        "{date} book_reductions {:?}",
        annotateds
            .iter()
            .map(|a| (&a.idx, &a.currency, &a.cost_currency, &a.price_currency,))
            .collect::<Vec<_>>()
    );

    for annotated in annotateds {
        let account = annotated.posting.account();
        let previous_positions = updated_inventory
            .get(&account)
            .or_else(|| inventory(account.clone()));
        let account_method = method(account.clone());
        let (costed_posting, updated_positions) = reduce(
            annotated,
            date,
            tolerance,
            account_method,
            previous_positions,
        )?;

        costed_postings.push(costed_posting);
        if let Some(updated_positions) = updated_positions {
            updated_inventory.insert(account, updated_positions);
        }
    }

    Ok(Reductions {
        updated_inventory: updated_inventory.into(),
        postings: costed_postings,
    })
}

fn reduce<'a, P, T>(
    annotated: AnnotatedPosting<P, P::Currency>,
    date: P::Date,
    tolerance: &T,
    method: Booking,
    previous_positions: Option<&Positions<P::Date, P::Number, P::Currency, P::Label>>,
) -> Result<
    (
        BookedOrUnbookedPosting<P>,
        Option<Positions<P::Date, P::Number, P::Currency, P::Label>>,
    ),
    BookingError,
>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
{
    use BookedOrUnbookedPosting::*;

    let account = annotated.posting.account();

    match (
        &annotated.currency,
        annotated.posting.units(),
        previous_positions,
    ) {
        (Some(posting_currency), Some(posting_units), Some(previous_positions)) => {
            // TODO booking methods other than strict
            // we already warn about this, so we simply ignore it here
            // if method != Booking::Strict {
            //     Err(BookingError::Transaction(
            //         TransactionBookingError::UnsupportedBookingMethod(
            //             method,
            //             account.to_string(),
            //         ),
            //     ))
            // } else
            tracing::debug!(
                "{date} reduce 1 {:?} {:?} {:?}",
                posting_currency,
                posting_units,
                previous_positions
            );

            if is_potential_reduction(posting_units, posting_currency, previous_positions) {
                // find positions whose costs match what we have
                let matched_positions =
                    match_positions(annotated.posting.cost().as_ref(), previous_positions);

                tracing::debug!(
                    "{date} reduce matched {:?} with {:?}",
                    &annotated,
                    &matched_positions
                );

                if matched_positions.is_empty() {
                    Err(BookingError::Posting(
                        annotated.idx,
                        PostingBookingError::NoPositionMatches,
                    ))
                } else if matched_positions.len() == 1 {
                    let (matched_position_idx, matched_position) =
                        matched_positions.into_iter().next().unwrap();
                    reduce_matched_position(
                        posting_units,
                        posting_currency,
                        annotated.posting,
                        annotated.idx,
                        previous_positions,
                        matched_position_idx,
                        matched_position,
                    )
                } else if is_sell_all_at_cost(
                    posting_units,
                    posting_currency,
                    previous_positions,
                    tolerance,
                ) {
                    let (reducing_posting, updated_positions) = reduce_all_sold_at_cost(
                        posting_units,
                        posting_currency,
                        annotated.posting,
                        annotated.idx,
                        previous_positions,
                        matched_positions,
                    )?;

                    Ok((reducing_posting, None))
                } else {
                    Err(BookingError::Posting(
                        annotated.idx,
                        PostingBookingError::AmbiguousMatches,
                    ))
                }
            } else {
                tracing::debug!(
                    "{date} reduce failed with {:?} {:?}",
                    posting_units.sign(),
                    previous_positions
                );

                Ok((Unbooked(annotated), None))
            }
        }
        x => {
            tracing::debug!("{date} reduce x {:?}", x,);

            Ok((Unbooked(annotated), None))
        }
    }
}

// do we found a position with cost and sign opposite to ours?
fn is_potential_reduction<D, N, C, L>(
    posting_units: N,
    posting_currency: &C,
    previous_positions: &Positions<D, N, C, L>,
) -> bool
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
{
    if let Some(ann_sign) = posting_units.sign()
        && previous_positions
            .iter()
            .filter(|pos| &pos.currency == posting_currency && pos.cost.is_some())
            .any(|pos| {
                pos.units
                    .sign()
                    .is_some_and(|pos_sign| pos_sign != ann_sign)
            })
    {
        true
    } else {
        false
    }
}

fn reduce_matched_position<'a, P>(
    posting_units: P::Number,
    posting_currency: &P::Currency,
    posting: P,
    posting_idx: usize,
    previous_positions: &Positions<P::Date, P::Number, P::Currency, P::Label>,
    matched_position_idx: usize,
    matched_position: &Position<P::Date, P::Number, P::Currency, P::Label>,
) -> Result<
    (
        BookedOrUnbookedPosting<P>,
        Option<Positions<P::Date, P::Number, P::Currency, P::Label>>,
    ),
    BookingError,
>
where
    P: PostingSpec + Debug + 'a,
{
    use BookedOrUnbookedPosting::*;

    let Position {
        currency: matched_currency,
        units: matched_units,
        cost: matched_cost,
    } = matched_position;

    tracing::debug!(
        "reduce cost-matched unique position at {}: {:?}",
        matched_position_idx,
        &matched_position
    );

    if posting_units.abs() > matched_units.abs() {
        Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::NotEnoughLotsToReduce,
        ))
    } else {
        // Book 'em, Danno!
        let matched_cost = matched_cost.as_ref().unwrap();
        let updated_positions = Positions::new(
            previous_positions
                .iter()
                .enumerate()
                .map(|(i, pos)| {
                    if i == matched_position_idx {
                        pos.with_accumulated(posting_units)
                    } else {
                        pos.clone()
                    }
                })
                .collect::<Vec<_>>(),
        );
        tracing::debug!("reduce_matched_position {:?}", &updated_positions);

        let date = matched_cost.date;
        let units = posting_units;
        let per_unit = matched_cost.per_unit;
        let label = matched_cost.label.as_ref().cloned();
        let merge = matched_cost.merge;

        Ok((
            Booked(Interpolated {
                posting,
                idx: posting_idx,
                units: posting_units,
                currency: posting_currency.clone(),
                cost: Some(PostingCosts {
                    cost_currency: matched_currency.clone(),
                    adjustments: vec![PostingCost {
                        date,
                        units: posting_units,
                        per_unit,
                        label,
                        merge,
                    }],
                }),
                // TODO price
                price: None,
            }),
            Some(updated_positions),
        ))
    }
}

// is this "sell everything"?
// that is, existing positions at cost together with this one sum to zero-ish updated_inventory
fn is_sell_all_at_cost<D, N, C, L, T>(
    posting_units: N,
    posting_currency: &C,
    previous_positions: &Positions<D, N, C, L>,
    tolerance: &T,
) -> bool
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
    T: Tolerance<Currency = C, Number = N>,
{
    tolerance
        .residual(
            previous_positions
                .iter()
                .filter_map(|pos| pos.cost.is_some().then_some(pos.units))
                .chain(once(posting_units)),
            posting_currency,
        )
        .is_none()
}

fn reduce_all_sold_at_cost<'a, P>(
    posting_units: P::Number,
    posting_currency: &P::Currency,
    posting: P,
    posting_idx: usize,
    previous_positions: &Positions<P::Date, P::Number, P::Currency, P::Label>,
    matched_positions: Vec<(usize, &Position<P::Date, P::Number, P::Currency, P::Label>)>,
) -> Result<
    (
        BookedOrUnbookedPosting<P>,
        Positions<P::Date, P::Number, P::Currency, P::Label>,
    ),
    BookingError,
>
where
    P: PostingSpec + Debug + 'a,
{
    use BookedOrUnbookedPosting::*;

    let cost_currencies = matched_positions
        .iter()
        .map(|(_i, pos)| pos.cost.as_ref().unwrap().currency.clone())
        .collect::<HashSet<_>>();

    if cost_currencies.len() == 1 {
        let cost_currency = cost_currencies.into_iter().next().unwrap();
        let cost_units: P::Number = matched_positions
            .iter()
            .map(|(_i, pos)| {
                (pos.cost.as_ref().unwrap().per_unit * posting_units)
                    .rescaled(posting_units.scale())
            })
            .sum();

        let matched_position_indices = matched_positions
            .iter()
            .map(|(i, _)| *i)
            .collect::<HashSet<_>>();

        let updated_positions = Positions::new(
            previous_positions
                .iter()
                .enumerate()
                .filter_map(|(i, pos)| matched_position_indices.contains(&i).then_some(pos.clone()))
                .collect::<Vec<_>>(),
        );

        Ok((
            Booked(Interpolated {
                posting,
                idx: posting_idx,
                units: posting_units,
                currency: posting_currency.clone(),
                cost: Some(PostingCosts {
                    cost_currency,
                    adjustments: matched_positions
                        .into_iter()
                        .map(|(idx, matched_pos)| {
                            let matched_cost = matched_pos.cost.clone().unwrap();
                            PostingCost {
                                date: matched_cost.date,
                                units: -matched_pos.units,
                                per_unit: matched_cost.per_unit,
                                label: matched_cost.label,
                                merge: matched_cost.merge,
                            }
                        })
                        .collect::<Vec<_>>(),
                }),
                // TODO price
                price: None,
            }),
            updated_positions,
        ))
    } else {
        Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::MultipleCostCurrenciesMatch,
        ))
    }
}

fn match_positions<'a, D, N, C, L, CS>(
    cost_spec: Option<&CS>,
    positions: &'a Positions<D, N, C, L>,
) -> Vec<(usize, &'a Position<D, N, C, L>)>
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
    CS: CostSpec<Date = D, Number = N, Currency = C, Label = L> + Debug,
{
    positions
        .iter()
        .enumerate()
        .filter_map(|(i, pos)| match (pos.cost.as_ref(), cost_spec) {
            (Some(pos_cost), Some(cost_spec)) => {
                tracing::debug!(
                    "match_positions check {:?} {:?} {}",
                    pos_cost,
                    cost_spec,
                    cost_matches_spec(pos_cost, cost_spec)
                );
                cost_matches_spec(pos_cost, cost_spec).then_some((i, pos))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
}

fn cost_matches_spec<D, N, C, L, CS>(cost: &Cost<D, N, C, L>, cost_spec: &CS) -> bool
where
    D: Eq + Copy,
    N: Eq + Copy,
    C: Eq + Clone,
    L: Eq + Clone,
    CS: CostSpec<Date = D, Number = N, Currency = C, Label = L>,
{
    !(
        cost_spec.date().is_some_and(|date| date != cost.date)
            || cost_spec
                .currency()
                .is_some_and(|cost_spec_currency| cost_spec_currency != cost.currency)
            || cost_spec
                .per_unit()
                .is_some_and(|cost_spec_units| cost_spec_units != cost.per_unit)
            || cost_spec
                .currency()
                .is_some_and(|cost_spec_currency| cost_spec_currency != cost.currency)
            || cost_spec
                .label()
                .is_some_and(|cost_spec_label| cost.label != Some(cost_spec_label))
        // TODO merge
    )
}
