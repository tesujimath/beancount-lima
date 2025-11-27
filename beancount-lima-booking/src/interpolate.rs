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
    pub(crate) booked_and_unbooked_postings: Vec<(
        Interpolated<P, P::Date, P::Number, P::Currency, P::Label>,
        bool, // booked
    )>,
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
    tracing::debug!(
        "interpolate_from_costed {date} {:?} {:?}",
        &currency,
        &costeds
    );
    let mut weights = costeds.iter().map(|c| c.weight()).collect::<Vec<_>>();
    let residual = tolerance.residual(weights.iter().filter_map(|w| *w), currency);
    tracing::debug!("{date} weights for {:?} {:?}", &currency, &weights);
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

    let booked_and_unbooked_postings = costeds
        .into_iter()
        .zip(weights)
        .map(|(c, w)| {
            match c {
                BookedOrUnbookedPosting::Unbooked(a) => {
                    let w = w.unwrap();

                    if a.posting.cost().is_none() && a.posting.price().is_none() {
                        // simple case with no cost or price
                        Ok((
                            Interpolated {
                                posting: a.posting,
                                idx: a.idx,
                                units: w,
                                currency: currency.clone(),
                                cost: None,
                                price: None,
                            },
                            false,
                        ))
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
                                Ok((
                                    Interpolated {
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
                                        price: None, // TODO price with cost
                                    },
                                    false,
                                ))
                            }
                            (
                                Some(UnitsAndCostPerUnit {
                                    units,
                                    cost_per_unit: _,
                                }),
                                Some(currency),
                                None,
                            ) => {
                                // TODO price without cost
                                tracing::debug!(
                                    "price without cost {} {} {} {:?}",
                                    a.idx,
                                    units,
                                    currency,
                                    a.posting.price()
                                );
                                Ok((
                                    Interpolated {
                                        posting: a.posting,
                                        idx: a.idx,
                                        units,
                                        currency,
                                        cost: None,
                                        price: None,
                                    },
                                    false,
                                ))
                            }

                            (None, Some(_), _) => Err(BookingError::Posting(
                                a.idx,
                                PostingBookingError::CannotInferUnits,
                            )),
                            (Some(_), None, _) => Err(BookingError::Posting(
                                a.idx,
                                PostingBookingError::CannotInferCurrency,
                            )),
                            (None, None, _) => Err(BookingError::Posting(
                                a.idx,
                                PostingBookingError::CannotInferAnything,
                            )),
                        }
                    }
                }
                BookedOrUnbookedPosting::Booked(i) => Ok((i, true)),
            }
        })
        .collect::<Result<Vec<_>, BookingError>>()?;

    Ok(Interpolation {
        booked_and_unbooked_postings,
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
    // TODO review unit inference from cost and price and weight
    if let Some(cost_spec) = posting.cost() {
        match (posting.units(), cost_spec.per_unit(), cost_spec.total()) {
            (Some(units), Some(cost_per_unit), _) => Some(UnitsAndCostPerUnit {
                units,
                cost_per_unit: Some(cost_per_unit),
            }),
            (None, Some(cost_per_unit), _) => Some(UnitsAndCostPerUnit {
                units: weight / cost_per_unit,
                cost_per_unit: Some(cost_per_unit),
            }),
            (Some(units), None, Some(cost_total)) => Some(UnitsAndCostPerUnit {
                units,
                cost_per_unit: Some(cost_total / units),
            }),
            (Some(units), None, None) => Some(UnitsAndCostPerUnit {
                units,
                cost_per_unit: None,
            }),
            (None, None, _) => None, // TODO is this correct?
        }
    } else if let Some(price_spec) = posting.price() {
        match (posting.units(), price_spec.per_unit(), price_spec.total()) {
            (Some(units), _, _) => Some(UnitsAndCostPerUnit {
                units,
                cost_per_unit: None,
            }),
            (None, Some(price_per_unit), _) => Some(UnitsAndCostPerUnit {
                units: weight / price_per_unit,
                cost_per_unit: None,
            }),
            (None, None, _) => None,
        }
    } else {
        posting.units().map(|units| UnitsAndCostPerUnit {
            units,
            cost_per_unit: None,
        })
    }
}
