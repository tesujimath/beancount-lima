// TODO remove dead code suppression
#![allow(dead_code)]

use std::fmt::Debug;

use super::{
    AnnotatedPosting, BookedOrUnbookedPosting, BookingError, CostSpec, Interpolated, Number,
    PostingBookingError, PostingCost, PostingCosts, PostingSpec, Price, PriceSpec, Tolerance,
    TransactionBookingError,
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

    pub(crate) residual: Option<P::Number>,
}

pub(crate) fn interpolate_from_costed<'a, 'b, P, T>(
    date: P::Date,
    currency: &P::Currency,
    costeds: Vec<BookedOrUnbookedPosting<P>>,
    tolerance: &T,
) -> Result<Interpolation<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
{
    let mut weights = costeds.iter().map(|c| c.weight()).collect::<Vec<_>>();
    let mut residual = tolerance.residual(weights.iter().filter_map(|w| *w), currency);
    tracing::debug!("{date} weights for {:?} {:?}", &currency, &weights);

    let unknown = weights
        .iter()
        .enumerate()
        .filter(|w| w.1.is_none())
        .collect::<Vec<_>>();
    tracing::debug!("{date} unknown values for {:?} {:?}", &currency, &unknown);

    if unknown.len() == 1 {
        let i_unknown = unknown[0].0;
        weights[i_unknown] = Some(-residual.unwrap_or_default());
        residual = None;
    } else if unknown.len() > 1 {
        return Err(BookingError::Transaction(
            TransactionBookingError::TooManyMissingNumbers,
        ));
    }

    let booked_and_unbooked_postings = costeds
        .into_iter()
        .zip(weights)
        .map(|(c, w)| match c {
            BookedOrUnbookedPosting::Unbooked(annotated) => {
                interpolate_from_annotated(date, currency, w.unwrap(), annotated)
            }

            BookedOrUnbookedPosting::Booked(i) => Ok((i, true)),
        })
        .collect::<Result<Vec<_>, BookingError>>()?;

    Ok(Interpolation {
        booked_and_unbooked_postings,
        residual,
    })
}

pub(crate) fn interpolate_from_annotated<'a, 'b, P>(
    date: P::Date,
    currency: &P::Currency,
    weight: P::Number,
    annotated: AnnotatedPosting<P, P::Currency>,
) -> Result<
    (
        Interpolated<P, P::Date, P::Number, P::Currency, P::Label>,
        bool, // booked
    ),
    BookingError,
>
where
    P: PostingSpec + Debug + 'a,
{
    match (
        units(&annotated.posting, weight),
        annotated.currency,
        annotated.posting.cost(),
        annotated.posting.price(),
    ) {
        (_, _, None, None) => {
            // simple case with no cost or price
            Ok((
                Interpolated {
                    posting: annotated.posting,
                    idx: annotated.idx,
                    units: weight,
                    currency: currency.clone(),
                    cost: None,
                    price: None,
                },
                false,
            ))
        }
        (Some(UnitsAndPerUnit { units, per_unit }), Some(currency), Some(cost), _) => {
            tracing::debug!(
                                    "{date} {currency} interpolate_from_annotated {units} {:?} annotated cost currency {:?}",
                                    &cost,
                                    annotated.cost_currency,
                                );
            match (annotated.cost_currency, per_unit) {
                (Some(cost_currency), Some(per_unit)) => Ok((
                    Interpolated {
                        posting: annotated.posting,
                        idx: annotated.idx,
                        units,
                        currency,
                        cost: Some(PostingCosts {
                            cost_currency,
                            adjustments: vec![PostingCost {
                                date: cost.date().unwrap_or(date),
                                units,
                                per_unit,
                                label: cost.label(),
                                merge: cost.merge(),
                            }],
                        }),
                        price: None, // ignored in favour of cost
                    },
                    false,
                )),
                (None, Some(_)) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferCurrency,
                )),
                (Some(_), None) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferUnits,
                )),
                (None, None) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferAnything,
                )),
            }
        }

        (Some(UnitsAndPerUnit { units, per_unit }), Some(currency), None, Some(price)) => {
            // price without cost
            tracing::debug!(
                "price without cost [{}] units: {} per-unit: {:?} currency: {} price: {:?}",
                annotated.idx,
                units,
                per_unit,
                currency,
                price
            );

            match (per_unit, annotated.price_currency) {
                (Some(per_unit), Some(price_currency)) => Ok((
                    Interpolated {
                        posting: annotated.posting,
                        idx: annotated.idx,
                        units,
                        currency,
                        cost: None,
                        price: Some(Price {
                            per_unit,
                            currency: price_currency,
                        }),
                    },
                    false,
                )),
                (None, Some(_)) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferPricePerUnit,
                )),
                (Some(_), None) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferPriceCurrency,
                )),
                (None, None) => Err(BookingError::Posting(
                    annotated.idx,
                    PostingBookingError::CannotInferPrice,
                )),
            }
        }

        (None, Some(_), _, _) => Err(BookingError::Posting(
            annotated.idx,
            PostingBookingError::CannotInferUnits,
        )),
        (Some(_), None, _, _) => Err(BookingError::Posting(
            annotated.idx,
            PostingBookingError::CannotInferCurrency,
        )),
        (None, None, _, _) => Err(BookingError::Posting(
            annotated.idx,
            PostingBookingError::CannotInferAnything,
        )),
    }
}

#[derive(Clone, Debug)]
struct UnitsAndPerUnit<N> {
    units: N,
    per_unit: Option<N>,
}

// infer the units once we know the weight
fn units<P>(posting: &P, weight: P::Number) -> Option<UnitsAndPerUnit<P::Number>>
where
    P: PostingSpec,
{
    // TODO review unit inference from cost and price and weight
    if let Some(cost_spec) = posting.cost() {
        units_from_cost_spec(posting.units(), weight, &cost_spec)
    } else if let Some(price_spec) = posting.price() {
        let u = units_from_price_spec(posting.units(), weight, &price_spec);
        tracing::debug!(
            "units_from_price_spec({:?}, {}, {:?}) = {:?}",
            posting.units(),
            weight,
            &price_spec,
            &u
        );
        u
    } else {
        posting.units().map(|units| UnitsAndPerUnit {
            units,
            per_unit: None,
        })
    }
}

fn units_from_cost_spec<D, N, C, L, CS>(
    posting_units: Option<N>,
    weight: N,
    cost_spec: &CS,
) -> Option<UnitsAndPerUnit<N>>
where
    D: Eq + Ord + Copy + Debug,
    N: Number + Debug,
    C: Eq + Ord + Clone + Debug,
    L: Eq + Ord + Clone + Debug,
    CS: CostSpec<Date = D, Number = N, Currency = C, Label = L> + Debug,
{
    match (posting_units, cost_spec.per_unit(), cost_spec.total()) {
        (Some(units), Some(per_unit), _) => Some(UnitsAndPerUnit {
            units,
            per_unit: Some(per_unit),
        }),
        (None, Some(per_unit), _) => {
            let units = (weight / per_unit).rescaled(weight.scale());
            Some(UnitsAndPerUnit {
                units,
                per_unit: Some(per_unit),
            })
        }
        (Some(units), None, Some(cost_total)) => {
            let per_unit = cost_total / units;
            Some(UnitsAndPerUnit {
                units,
                per_unit: Some(per_unit),
            })
        }
        (Some(units), None, None) => Some(UnitsAndPerUnit {
            units,
            per_unit: None,
        }),
        (None, None, _) => None, // TODO is this correct?
    }
}

fn units_from_price_spec<N, C, PS>(
    posting_units: Option<N>,
    weight: N,
    price_spec: &PS,
) -> Option<UnitsAndPerUnit<N>>
where
    N: Number + Debug,
    C: Eq + Ord + Clone + Debug,
    PS: PriceSpec<Number = N, Currency = C> + Debug,
{
    match (posting_units, price_spec.per_unit(), price_spec.total()) {
        (Some(units), Some(per_unit), _) => Some(UnitsAndPerUnit {
            units,
            per_unit: Some(per_unit),
        }),
        (None, Some(per_unit), _) => {
            let units = (weight / per_unit).rescaled(weight.scale());
            Some(UnitsAndPerUnit {
                units,
                per_unit: Some(per_unit),
            })
        }
        (Some(units), None, Some(total)) => {
            let per_unit = total / units;
            Some(UnitsAndPerUnit {
                units,
                per_unit: Some(per_unit),
            })
        }
        (Some(units), None, None) => {
            let per_unit = weight / units;
            Some(UnitsAndPerUnit {
                units,
                per_unit: Some(per_unit),
            })
        }
        (None, None, _) => None,
    }
}
