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
    positions: Option<&Positions<P::Date, P::Number, P::Currency, P::Label>>,
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

    match (&annotated.currency, annotated.posting.units(), positions) {
        (Some(posting_currency), Some(posting_units), Some(positions))
            if method != Booking::None =>
        {
            tracing::debug!(
                "{date} reduce 1 {method} {:?} {:?} {:?}",
                posting_currency,
                posting_units,
                positions
            );

            if annotated.posting.cost().is_some()
                && is_potential_reduction(posting_units, posting_currency, positions)
            {
                // find positions whose costs match what we have
                let matched = match_positions(
                    posting_currency,
                    annotated.posting.cost().as_ref(),
                    positions,
                );

                tracing::debug!("{date} reduce {method} matched with {:?}", &matched);

                if matched.is_empty() {
                    Err(BookingError::Posting(
                        annotated.idx,
                        PostingBookingError::NoPositionMatches,
                    ))
                } else if matched.len() == 1 {
                    let (reducing_posting, updated_positions) = reduce_matched_position(
                        posting_units,
                        posting_currency,
                        annotated.posting,
                        annotated.idx,
                        positions,
                        matched[0],
                    )?;

                    Ok((reducing_posting, Some(updated_positions)))
                } else if is_sell_all_at_cost(
                    posting_units,
                    posting_currency,
                    positions,
                    &matched,
                    tolerance,
                ) {
                    tracing::debug!("{date} reduce_all_sold_at_cost {method}");
                    let (reducing_posting, updated_positions) = reduce_all_sold_at_cost(
                        posting_units,
                        posting_currency,
                        annotated.posting,
                        annotated.idx,
                        positions,
                        matched,
                    )?;

                    Ok((reducing_posting, Some(updated_positions)))
                } else {
                    tracing::debug!("{date} reduce_multiple_positions {method}");
                    let (reducing_posting, updated_positions) = reduce_multiple_positions(
                        posting_units,
                        posting_currency,
                        annotated.posting,
                        annotated.idx,
                        positions,
                        matched,
                        method,
                    )?;

                    Ok((reducing_posting, Some(updated_positions)))
                }
            } else {
                tracing::debug!(
                    "{date} reduce failed with {:?} {:?}",
                    posting_units.sign(),
                    positions
                );

                Ok((Unbooked(annotated), None))
            }
        }
        x => {
            tracing::debug!("{date} reduce x {method} {:?}", x,);

            Ok((Unbooked(annotated), None))
        }
    }
}

// do any positions in this currency have a sign opposite to ours?
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
            .filter(|pos| &pos.currency == posting_currency)
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

    let Position {
        currency: matched_currency,
        units: matched_units,
        cost: matched_cost,
    } = &previous_positions[matched_position_idx];

    tracing::debug!(
        "reduce cost-matched unique position at {}: {:?} {:?} {:?}",
        matched_position_idx,
        matched_currency,
        matched_units,
        matched_cost
    );

    if posting_units.abs() > matched_units.abs() {
        Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::NotEnoughLotsToReduce,
        ))
    } else {
        // Book 'em, Danno!
        let matched_cost = matched_cost.as_ref().unwrap();
        let updated_positions = Positions::from_previous(
            previous_positions
                .iter()
                .enumerate()
                .filter_map(|(i, pos)| {
                    if i == matched_position_idx {
                        let updated_pos = pos.with_accumulated(posting_units);
                        (updated_pos.units != Number::zero()).then_some(updated_pos)
                    } else {
                        Some(pos.clone())
                    }
                })
                .collect::<Vec<_>>(),
        );
        tracing::debug!("reduce_matched_position {:?}", &updated_positions);

        let date = matched_cost.date;
        let units = posting_units;
        let per_unit = matched_cost.per_unit;
        let cost_currency = matched_cost.currency.clone();
        let label = matched_cost.label.as_ref().cloned();
        let merge = matched_cost.merge;

        Ok((
            Booked(Interpolated {
                posting,
                idx: posting_idx,
                units: posting_units,
                currency: posting_currency.clone(),
                cost: Some(PostingCosts {
                    cost_currency,
                    adjustments: vec![PostingCost {
                        date,
                        units: posting_units,
                        per_unit,
                        label,
                        merge,
                    }],
                }),
                price: None, // ignored in favour of cost
            }),
            updated_positions,
        ))
    }
}

// is this "sell everything that matches"?
// that is, matched positions together with this one sum to zero-ish updated_inventory
fn is_sell_all_at_cost<D, N, C, L, T>(
    posting_units: N,
    posting_currency: &C,
    positions: &Positions<D, N, C, L>,
    matched: &[usize],
    tolerance: &T,
) -> bool
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
    T: Tolerance<Currency = C, Number = N>,
{
    let tol = tolerance.residual(
        matched
            .iter()
            .map(|i| positions[*i].units)
            .chain(once(posting_units)),
        posting_currency,
    );
    tracing::debug!(
        "is_sell_all_at_cost {:?} with {:?} matched {:?} tol is {:?}",
        posting_units,
        positions,
        matched,
        &tol
    );
    tol.is_none()
}

fn reduce_multiple_positions<'a, P>(
    posting_units: P::Number,
    posting_currency: &P::Currency,
    posting: P,
    posting_idx: usize,
    positions: &Positions<P::Date, P::Number, P::Currency, P::Label>,
    mut matched: Vec<usize>,
    method: Booking,
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
    match method {
        Booking::Fifo | Booking::Lifo | Booking::Hifo => {
            check_sufficient_matched_units(posting_units, posting_idx, positions, &matched)?;
            let cost_currency = get_unique_cost_currency(posting_idx, positions, &matched)?;

            // all that differs between the [FLH]ifo methods is the order in which we select matched postings for reduction
            if method == Booking::Lifo {
                matched.reverse();
            } else if method == Booking::Hifo {
                // sort by cost per-unit, greater first
                matched.sort_by(|i, j| {
                    positions[*j]
                        .cost
                        .as_ref()
                        .unwrap()
                        .per_unit
                        .cmp(&positions[*i].cost.as_ref().unwrap().per_unit)
                });
            }

            reduce_ordered_positions(
                posting_units,
                posting_currency.clone(),
                cost_currency,
                posting,
                posting_idx,
                positions,
                &matched,
            )
        }

        Booking::StrictWithSize => {
            // not only do we filter to positions which match the posting units, but we take the oldest by cost date
            let mut matched_with_size = matched
                .into_iter()
                .filter(|i| positions[*i].units == -posting_units)
                .collect::<Vec<_>>();
            matched_with_size.sort_by(|i, j| {
                positions[*i]
                    .cost
                    .as_ref()
                    .unwrap()
                    .date
                    .cmp(&positions[*j].cost.as_ref().unwrap().date)
            });

            if !matched_with_size.is_empty() {
                reduce_matched_position(
                    posting_units,
                    posting_currency,
                    posting,
                    posting_idx,
                    positions,
                    matched_with_size[0],
                )
            } else {
                Err(BookingError::Posting(
                    posting_idx,
                    PostingBookingError::AmbiguousMatches,
                ))
            }
        }

        _ => Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::AmbiguousMatches,
        )),
    }
}

fn reduce_ordered_positions<'a, P>(
    posting_units: P::Number,
    posting_currency: P::Currency,
    cost_currency: P::Currency,
    posting: P,
    posting_idx: usize,
    positions: &Positions<P::Date, P::Number, P::Currency, P::Label>,
    matched: &[usize],
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

    let mut remaining_units = posting_units;
    let mut updated_position_units = positions.iter().map(|p| p.units).collect::<Vec<_>>();
    let mut adjustments = Vec::default();

    tracing::debug!(
        "reduce_ordered_positions {:?} being {:?}",
        &matched,
        matched.iter().map(|i| &positions[*i]).collect::<Vec<_>>()
    );

    for i in matched {
        let cost_i = positions[*i].cost.as_ref().unwrap();
        let consumed = if remaining_units.abs() <= updated_position_units[*i].abs() {
            remaining_units
        } else {
            -updated_position_units[*i]
        };

        tracing::debug!(
            "with {} remaining, consuming {} of {} leaving {} with {} still to consume",
            remaining_units,
            consumed,
            updated_position_units[*i],
            updated_position_units[*i] + consumed,
            remaining_units - consumed
        );

        updated_position_units[*i] += consumed;
        remaining_units -= consumed;

        adjustments.push(PostingCost {
            date: cost_i.date,
            units: consumed,
            per_unit: cost_i.per_unit,
            label: cost_i.label.as_ref().cloned(),
            merge: cost_i.merge,
        });

        if remaining_units == P::Number::zero() {
            break;
        }
    }

    if remaining_units != P::Number::zero() {
        return Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::NotEnoughLotsToReduce,
        ));
    }

    let updated_positions = Positions::from_previous(
        updated_position_units
            .into_iter()
            .enumerate()
            .filter_map(|(i, units)| {
                let position = &positions[i];
                (units != P::Number::zero()).then_some(Position {
                    currency: posting_currency.clone(),
                    units,
                    cost: position.cost.clone(),
                })
            })
            .collect::<Vec<_>>(),
    );

    tracing::debug!(
        "reduce_ordered_positions returning {:?} {:?}",
        &adjustments,
        &updated_positions
    );

    Ok((
        Booked(Interpolated {
            posting,
            idx: posting_idx,
            units: posting_units,
            currency: posting_currency,
            cost: Some(PostingCosts {
                cost_currency,
                adjustments,
            }),
            price: None, // ignored in favour of cost
        }),
        updated_positions,
    ))
}

fn check_sufficient_matched_units<D, N, C, L>(
    posting_units: N,
    posting_idx: usize,
    positions: &Positions<D, N, C, L>,
    matched: &[usize],
) -> Result<(), BookingError>
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
{
    let total_matched_units: N = matched.iter().map(|i| positions[*i].units).sum();

    if posting_units <= total_matched_units {
        Ok(())
    } else {
        Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::NotEnoughLotsToReduce,
        ))
    }
}

fn reduce_all_sold_at_cost<'a, P>(
    posting_units: P::Number,
    posting_currency: &P::Currency,
    posting: P,
    posting_idx: usize,
    positions: &Positions<P::Date, P::Number, P::Currency, P::Label>,
    matched: Vec<usize>,
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

    let cost_currency = get_unique_cost_currency(posting_idx, positions, &matched)?;
    let cost_units: P::Number = matched
        .iter()
        .map(|i| {
            (positions[*i].cost.as_ref().unwrap().per_unit * posting_units)
                .rescaled(posting_units.scale())
        })
        .sum();

    let matched_set = matched.iter().copied().collect::<HashSet<_>>();

    let updated_positions = Positions::from_previous(
        positions
            .iter()
            .enumerate()
            .filter_map(|(i, pos)| (!matched_set.contains(&i)).then_some(pos.clone()))
            .collect::<Vec<_>>(),
    );
    let adjustments = matched
        .iter()
        .map(|i| {
            let matched_position = &positions[*i];
            let matched_cost = matched_position.cost.clone().unwrap();
            PostingCost {
                date: matched_cost.date,
                units: -matched_position.units,
                per_unit: matched_cost.per_unit,
                label: matched_cost.label,
                merge: matched_cost.merge,
            }
        })
        .collect::<Vec<_>>();

    Ok((
        Booked(Interpolated {
            posting,
            idx: posting_idx,
            units: posting_units,
            currency: posting_currency.clone(),
            cost: Some(PostingCosts {
                cost_currency,
                adjustments,
            }),
            price: None, // ignored in favour of cost
        }),
        updated_positions,
    ))
}

fn get_unique_cost_currency<D, N, C, L>(
    posting_idx: usize,
    positions: &Positions<D, N, C, L>,
    matched: &[usize],
) -> Result<C, BookingError>
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Hash + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
{
    let cost_currencies = matched
        .iter()
        .map(|i| positions[*i].cost.as_ref().unwrap().currency.clone())
        .collect::<HashSet<_>>();

    if cost_currencies.len() == 1 {
        let cost_currency = cost_currencies.into_iter().next().unwrap();
        Ok(cost_currency)
    } else {
        Err(BookingError::Posting(
            posting_idx,
            PostingBookingError::MultipleCostCurrenciesMatch,
        ))
    }
}

fn match_positions<D, N, C, L, CS>(
    posting_currency: &C,
    cost_spec: Option<&CS>,
    positions: &Positions<D, N, C, L>,
) -> Vec<usize>
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
        .filter_map(|(i, pos)| {
            if &pos.currency != posting_currency {
                None
            } else {
                match (pos.cost.as_ref(), cost_spec) {
                    (Some(pos_cost), Some(cost_spec)) => {
                        tracing::debug!(
                            "match_positions check {:?} {:?} {}",
                            pos_cost,
                            cost_spec,
                            cost_matches_spec(pos_cost, cost_spec)
                        );
                        cost_matches_spec(pos_cost, cost_spec).then_some(i)
                    }
                    _ => None,
                }
            }
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
