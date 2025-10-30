// TODO remove dead code suppression
#![allow(dead_code)]

use std::fmt::Debug;

use super::{
    BookingError, CostedPosting, Posting, Tolerance, TransactionBookingError, WeightedPosting,
};

#[derive(Debug)]
pub(crate) struct Interpolation<'p, P, N, C>
where
    N: Copy,
    C: Clone,
{
    weighted_postings: Vec<WeightedPosting<'p, P, N, C>>,
}

pub(crate) fn interpolate<'p, 'i, 'b, P, T>(
    currency: &P::Currency,
    costeds: Vec<CostedPosting<'p, P, P::Number, P::Currency>>,
    tolerance: &T,
) -> Result<Interpolation<'p, P, P::Number, P::Currency>, BookingError>
where
    P: Posting + Debug + 'p + 'i,
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

                Some(WeightedPosting {
                    posting: a.posting,
                    idx: a.idx,
                    units: w,
                    cost_currency: a.cost_currency,
                    price_currency: a.price_currency,
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    Ok(Interpolation { weighted_postings })
}
