// TODO remove dead code suppression
#![allow(dead_code)]

use std::fmt::Debug;

use super::{
    BookingError, CostedPosting, InterpolatedCost, InterpolatedPosting, Posting,
    PostingBookingError, Tolerance, TransactionBookingError,
};

#[derive(Debug)]
pub(crate) struct Interpolation<P, N, C>
where
    N: Copy,
    C: Clone,
{
    pub(crate) unbooked_postings: Vec<InterpolatedPosting<P, N, C>>,
}

pub(crate) fn interpolate<'i, 'b, P, T>(
    currency: &P::Currency,
    costeds: Vec<CostedPosting<P, P::Number, P::Currency>>,
    tolerance: &T,
) -> Result<Interpolation<P, P::Number, P::Currency>, BookingError>
where
    P: Posting + Debug + 'i,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
{
    let mut weights = costeds.iter().map(|c| c.weight()).collect::<Vec<_>>();
    let residual = tolerance.residual(weights.iter().filter_map(|w| *w), currency);
    let unknown = weights
        .iter()
        .enumerate()
        .filter(|w| w.1.is_none())
        .collect::<Vec<_>>();
    match (residual, unknown.len()) {
        (None, 0) => (),
        (None, 1) => Err(BookingError::Transaction(
            TransactionBookingError::NoResidualForInterpolation,
        ))?,
        (Some(residual), 0) => Err(BookingError::Transaction(
            TransactionBookingError::Unbalanced(residual.to_string()),
        ))?,
        (Some(residual), 1) => {
            let i_unknown = unknown[0].0;
            weights[i_unknown] = Some(-residual);
        }
        _ => Err(BookingError::Transaction(
            TransactionBookingError::TooManyMissingNumbers,
        ))?,
    }

    // only return weighted postings for the unbooked
    let weighted_postings = costeds
        .into_iter()
        .zip(weights)
        .filter_map(|(c, w)| {
            if let CostedPosting::Unbooked(a) = c {
                let w = w.unwrap();

                if !(a.posting.has_cost() || a.posting.has_price()) {
                    // simple case with no cost or price
                    Some(Ok(InterpolatedPosting {
                        posting: a.posting,
                        idx: a.idx,
                        units: w,
                        currency: currency.clone(),
                        cost: None,
                    }))
                } else {
                    match (units(&a.posting, w), a.currency) {
                        (
                            Some(UnitsAndCostPerUnit {
                                units,
                                cost_per_unit,
                            }),
                            Some(currency),
                        ) => {
                            if a.posting.has_cost() {
                                Some(Ok(InterpolatedPosting {
                                    posting: a.posting,
                                    idx: a.idx,
                                    units,
                                    currency,
                                    cost: Some(InterpolatedCost {
                                        // I don't think these can fail, but let's see during testing:
                                        per_unit: cost_per_unit.unwrap(),
                                        currency: a.cost_currency.unwrap(),
                                    }),
                                }))
                            } else {
                                // price
                                Some(Ok(InterpolatedPosting {
                                    posting: a.posting,
                                    idx: a.idx,
                                    units,
                                    currency,
                                    cost: None,
                                }))
                            }
                        }
                        (None, Some(_)) => Some(Err(BookingError::Posting(
                            a.idx,
                            PostingBookingError::CannotInferUnits,
                        ))),
                        (Some(_), None) => Some(Err(BookingError::Posting(
                            a.idx,
                            PostingBookingError::CannotInferCurrency,
                        ))),
                        (None, None) => Some(Err(BookingError::Posting(
                            a.idx,
                            PostingBookingError::CannotInferAnything,
                        ))),
                    }
                }
            } else {
                None
            }
        })
        .collect::<Result<Vec<_>, BookingError>>()?;

    Ok(Interpolation {
        unbooked_postings: weighted_postings,
    })
}

struct UnitsAndCostPerUnit<N> {
    units: N,
    cost_per_unit: Option<N>,
}

// infer the units once we know the weight
fn units<P>(posting: &P, weight: P::Number) -> Option<UnitsAndCostPerUnit<P::Number>>
where
    P: Posting,
{
    let units = posting.units().unwrap_or(weight);
    if posting.has_cost() {
        posting
            .cost_per_unit()
            .map(|cost_per_unit| UnitsAndCostPerUnit {
                units: units / cost_per_unit,
                cost_per_unit: Some(cost_per_unit),
            })
    } else if posting.has_price() {
        posting
            .price_per_unit()
            .map(|price_per_unit| UnitsAndCostPerUnit {
                units: units / price_per_unit,
                cost_per_unit: None,
            })
    } else {
        Some(UnitsAndCostPerUnit {
            units,
            cost_per_unit: None,
        })
    }
}
