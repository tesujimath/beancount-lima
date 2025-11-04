// TODO remove dead code suppression
#![allow(dead_code)]

use std::fmt::Debug;

use super::{
    BookedOrUnbookedPosting, BookingError, CostSpec, Interpolated, PostingBookingError,
    PostingCost, PostingCosts, PostingSpec, PriceSpec, Tolerance, TransactionBookingError,
};

#[derive(Debug)]
pub(crate) struct Interpolation<P>
where
    P: PostingSpec,
{
    pub(crate) unbooked_postings: Vec<Interpolated<P, P::Date, P::Number, P::Currency, P::Label>>,
}

pub(crate) fn interpolate_from_costed<'i, 'b, P, T>(
    date: P::Date,
    currency: &P::Currency,
    costeds: Vec<BookedOrUnbookedPosting<P>>,
    tolerance: &T,
) -> Result<Interpolation<P>, BookingError>
where
    P: PostingSpec + Debug + 'i,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
{
    let mut weights = costeds.iter().map(|c| c.weight()).collect::<Vec<_>>();
    let residual = tolerance.residual(weights.iter().filter_map(|w| *w), currency);
    tracing::debug!("weights for {:?} {:?}", &currency, &weights);
    let unknown = weights
        .iter()
        .enumerate()
        .filter(|w| w.1.is_none())
        .collect::<Vec<_>>();
    tracing::debug!("unknown values for {:?} {:?}", &currency, &unknown);
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
            if let BookedOrUnbookedPosting::Unbooked(a) = c {
                let w = w.unwrap();

                if a.posting.cost().is_none() && a.posting.price().is_none() {
                    // simple case with no cost or price
                    Some(Ok(Interpolated {
                        posting: a.posting,
                        idx: a.idx,
                        units: w,
                        currency: currency.clone(),
                        cost: None,
                        price: None,
                    }))
                } else {
                    match (units(&a.posting, w), a.currency, a.posting.cost()) {
                        (
                            Some(UnitsAndCostPerUnit {
                                units,
                                cost_per_unit,
                            }),
                            Some(currency),
                            Some(cost),
                        ) => {
                            Some(Ok(Interpolated {
                                posting: a.posting,
                                idx: a.idx,
                                units,
                                currency,
                                cost: Some(PostingCosts {
                                    cost_currency: a.cost_currency.unwrap(),
                                    adjustments: vec![PostingCost {
                                        date,
                                        units,
                                        per_unit: (cost_per_unit.unwrap()), // can't fail, since we have cost
                                        label: cost.label(),
                                        merge: cost.merge(),
                                    }],
                                    // I don't think these can fail, but let's see during testing:
                                    // per_unit: cost_per_unit.unwrap(),
                                    // currency: a.cost_currency.unwrap(),
                                }),
                                price: None, // TODO price
                            }))
                        }
                        (
                            Some(UnitsAndCostPerUnit {
                                units,
                                cost_per_unit: _,
                            }),
                            Some(currency),
                            None,
                        ) => {
                            // TODO price
                            Some(Ok(Interpolated {
                                posting: a.posting,
                                idx: a.idx,
                                units,
                                currency,
                                cost: None,
                                price: None,
                            }))
                        }

                        (None, Some(_), _) => Some(Err(BookingError::Posting(
                            a.idx,
                            PostingBookingError::CannotInferUnits,
                        ))),
                        (Some(_), None, _) => Some(Err(BookingError::Posting(
                            a.idx,
                            PostingBookingError::CannotInferCurrency,
                        ))),
                        (None, None, _) => Some(Err(BookingError::Posting(
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
    P: PostingSpec,
{
    let units = posting.units().unwrap_or(weight);
    if let Some(cost_spec) = posting.cost() {
        cost_spec
            .per_unit()
            .map(|cost_per_unit| UnitsAndCostPerUnit {
                units: units / cost_per_unit,
                cost_per_unit: Some(cost_per_unit),
            })
    } else if let Some(price_spec) = posting.price() {
        price_spec
            .per_unit()
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
