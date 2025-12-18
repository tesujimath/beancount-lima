// TODO remove dead code suppression
#![allow(dead_code, unused_variables)]

use hashbrown::HashMap;
use std::{fmt::Debug, iter::repeat_n};

use super::{
    book_reductions, categorize_by_currency, interpolate_from_costed, Booking, BookingError,
    Bookings, Interpolated, Interpolation, Inventory, Positions, Posting, PostingSpec, Reductions,
    Tolerance, TransactionBookingError,
};

pub fn is_supported_method(method: Booking) -> bool {
    use Booking::*;

    match method {
        Strict => true,
        StrictWithSize => true,
        None => true,
        Average => false,
        Fifo => true,
        Lifo => true,
        Hifo => true,
    }
}

/// Book the postings for the given date, returning updated inventory and interpolated postings.
/// The interpolated postings are aligned with the original postings, in that they may be zipped together and
/// will always correspond.
pub fn book<'a, 'b, P, T, I, M>(
    date: P::Date,
    postings: &[P],
    tolerance: &'b T,
    inventory: I,
    method: M,
) -> Result<Bookings<P>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'b Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    M: Fn(P::Account) -> Booking + Copy,
    'a: 'b,
{
    let (bookings, residuals) = book_with_residuals(date, postings, tolerance, inventory, method)?;
    if !residuals.is_empty() {
        let mut currencies = residuals.keys().collect::<Vec<_>>();
        currencies.sort();
        let message = currencies
            .into_iter()
            .map(|cur| format!("{} {}", -*residuals.get(cur).unwrap(), cur))
            .collect::<Vec<String>>()
            .join(", ");
        return Err(BookingError::Transaction(
            TransactionBookingError::Unbalanced(message),
        ));
    }

    Ok(bookings)
}

pub(crate) type Residuals<C, N> = HashMap<C, N>;

// this exists so we can test the booking algorithm with unbalanced transactions
// as per OG Beancount booking_full_test.py
pub(crate) fn book_with_residuals<'a, 'b, P, T, I, M>(
    date: P::Date,
    postings: &[P],
    tolerance: &'b T,
    inventory: I,
    method: M,
) -> Result<(Bookings<P>, Residuals<P::Currency, P::Number>), BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'b Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    M: Fn(P::Account) -> Booking + Copy,
    'a: 'b,
{
    let mut interpolated_postings = repeat_n(None, postings.len()).collect::<Vec<_>>();
    let mut updated_inventory = Inventory::default();

    let currency_groups = categorize_by_currency(postings, inventory)?;
    let mut residuals = Residuals::<P::Currency, P::Number>::default();

    for (cur, annotated_postings) in currency_groups {
        let Reductions {
            updated_inventory: updated_inventory_for_cur,
            postings: costed_postings,
        } = book_reductions(
            date,
            annotated_postings,
            tolerance,
            |account| {
                updated_inventory
                    .get(&account)
                    .or_else(|| inventory(account.clone()))
            },
            method,
        )?;

        tracing::debug!(
            "{date} booked reductions {:?} {:?}",
            &cur,
            updated_inventory_for_cur
        );
        for (account, positions) in updated_inventory_for_cur {
            updated_inventory.insert(account, positions);
        }

        let Interpolation {
            booked_and_unbooked_postings,
            residual,
        } = interpolate_from_costed(date, &cur, costed_postings, tolerance)?;

        if let Some(residual) = residual {
            residuals.insert(cur.clone(), residual);
        }

        let updated_inventory_for_cur = book_augmentations(
            date,
            booked_and_unbooked_postings
                .iter()
                .filter_map(|(p, booked)| (!booked).then_some(p)),
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

        for (p, _) in booked_and_unbooked_postings.into_iter() {
            let idx = p.idx;
            interpolated_postings[idx] = Some(p);
        }
    }

    let interpolated_postings = interpolated_postings
        .into_iter()
        .map(|p| p.unwrap())
        .collect::<Vec<_>>();

    tracing::debug!(
        "book_with_residuals updated inventory {:?}",
        &updated_inventory
    );

    Ok((
        Bookings {
            interpolated_postings,
            updated_inventory,
        },
        residuals,
    ))
}

/// book without the need for interpolation
pub fn accumulate<'a, P, I, M>(
    date: P::Date,
    postings: impl Iterator<Item = P>,
    inventory: I,
    method: M,
) -> Result<Inventory<P::Account, P::Date, P::Number, P::Currency, P::Label>, BookingError>
where
    P: Posting + Debug + 'a,
    I: Fn(P::Account) -> Option<&'a Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    M: Fn(P::Account) -> Booking + Copy,
{
    let mut updated_inventory = HashMap::default();

    for posting in postings {
        use hashbrown::hash_map::Entry::*;

        let account = posting.account();
        let account_method = method(account.clone());

        let previous_positions = match updated_inventory.entry(account.clone()) {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(inventory(account).cloned().unwrap_or_default()),
        };
        // .or_else(|| inventory(account.clone()));

        match posting.cost() {
            None => {
                previous_positions.accumulate(
                    posting.units(),
                    posting.currency(),
                    None,
                    account_method,
                );
            }
            Some(costs) => {
                for (cur, adj) in costs.iter() {
                    previous_positions.accumulate(
                        adj.units,
                        posting.currency(),
                        Some((cur.clone(), adj.clone()).into()),
                        account_method,
                    );
                }
            }
        }
    }

    Ok(updated_inventory.into())
}

fn book_augmentations<'a, 'b, P, T, I, M>(
    date: P::Date,
    interpolateds: impl Iterator<Item = &'b Interpolated<P, P::Date, P::Number, P::Currency, P::Label>>,
    tolerance: &T,
    inventory: I,
    method: M,
) -> Result<Inventory<P::Account, P::Date, P::Number, P::Currency, P::Label>, BookingError>
where
    P: PostingSpec + Debug + 'a,
    T: Tolerance<Currency = P::Currency, Number = P::Number>,
    I: Fn(P::Account) -> Option<&'a Positions<P::Date, P::Number, P::Currency, P::Label>> + Copy,
    M: Fn(P::Account) -> Booking + Copy,
    'a: 'b,
{
    let mut updated_inventory = HashMap::default();

    for interpolated in interpolateds {
        use hashbrown::hash_map::Entry::*;

        let posting = &interpolated.posting;
        let account = posting.account();
        let account_method = method(account.clone());

        let previous_positions = match updated_inventory.entry(account.clone()) {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(inventory(account).cloned().unwrap_or_default()),
        };
        // .or_else(|| inventory(account.clone()));

        if let Some(posting_costs) = interpolated.cost.as_ref() {
            tracing::debug!(
                "{date} book_augmentations with cost {:?} {:?} {:?}",
                interpolated.units,
                &interpolated.currency,
                &posting_costs,
            );
            for (currency, posting_cost) in posting_costs.iter() {
                previous_positions.accumulate(
                    interpolated.units,
                    interpolated.currency.clone(),
                    Some((currency.clone(), posting_cost.clone()).into()),
                    account_method,
                );
            }
        } else {
            tracing::debug!(
                "{date} book_augmentations without cost {:?} {:?}",
                interpolated.units,
                &interpolated.currency,
            );
            previous_positions.accumulate(
                interpolated.units,
                interpolated.currency.clone(),
                None,
                account_method,
            );
        }
    }
    Ok(updated_inventory.into())
}
