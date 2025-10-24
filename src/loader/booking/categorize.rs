use hashbrown::{hash_map::Entry, HashMap};
use std::ops::Deref;

use super::{super::util::*, *};

#[derive(Clone, Debug)]
pub(crate) struct CurrenciedPosting<'a> {
    posting: &'a parser::Spanned<parser::Posting<'a>>,
    units_currency: Option<&'a parser::Currency<'a>>,
    cost_currency: Option<&'a parser::Currency<'a>>,
    price_currency: Option<&'a parser::Currency<'a>>,
}

impl<'a> CurrenciedPosting<'a> {
    fn bucket(&self) -> Option<&'a parser::Currency<'a>> {
        self.cost_currency
            .or(self.price_currency)
            .or(self.units_currency)
    }
}

pub(crate) trait AccountCurrency<'a> {
    fn units(&self, account: &'a str) -> Option<&'a parser::Currency<'a>>;
    fn cost(&self, account: &'a str) -> Option<&'a parser::Currency<'a>>;
}

#[derive(Default, Clone, Debug)]
pub(crate) struct CategorizedPostings<'a>(
    HashMap<&'a parser::Currency<'a>, Vec<CurrenciedPosting<'a>>>,
);

impl<'a> CategorizedPostings<'a> {
    fn insert(&mut self, bucket: &'a parser::Currency<'a>, p: CurrenciedPosting<'a>) {
        use Entry::*;

        match self.0.entry(bucket) {
            Occupied(mut occupied) => {
                occupied.get_mut().push(p);
            }
            Vacant(vacant) => {
                vacant.insert(vec![p]);
            }
        }
    }
}

impl<'a> Deref for CategorizedPostings<'a> {
    type Target = HashMap<&'a parser::Currency<'a>, Vec<CurrenciedPosting<'a>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// See OG Beancount function of the same name
pub(crate) fn categorize_by_currency<'a, P, A>(
    postings: P,
    account_currency_lookup: A,
) -> Result<CategorizedPostings<'a>, parser::AnnotatedError>
where
    P: IntoIterator<Item = &'a parser::Spanned<parser::Posting<'a>>>,
    A: AccountCurrency<'a>,
{
    let mut groups = CategorizedPostings::default();
    let mut auto_postings = Vec::default();
    let mut unknown = Vec::default();

    for posting in postings {
        let units_currency = posting.currency().map(parser::Spanned::item);
        let posting_cost_currency = posting
            .cost_spec()
            .and_then(|cost_spec| cost_spec_currency(cost_spec.item()));
        let posting_price_currency = posting
            .price_annotation()
            .and_then(|price| price_spec_currency(price.item()));
        let cost_currency = posting_cost_currency.or(posting_price_currency);
        let price_currency = posting_price_currency.or(posting_cost_currency);

        let p = CurrenciedPosting {
            posting,
            units_currency,
            cost_currency,
            price_currency,
        };

        if posting.amount().is_none() && posting.currency().is_none() {
            auto_postings.push(p);
        } else if let Some(bucket) = p.bucket() {
            groups.insert(bucket, p);
        } else {
            unknown.push(p);
        }
    }

    // if we have a single unknown posting and all others are of the same currency,
    // infer that for the unknown
    if unknown.len() == 1 && groups.len() == 1 {
        let only_bucket = *groups.keys().next().unwrap();
        let u = unknown.drain(..).next().unwrap();
        let inferred = CurrenciedPosting {
            posting: u.posting,
            units_currency: if u.price_currency.is_none() && u.cost_currency.is_none() {
                Some(only_bucket)
            } else {
                None
            },
            cost_currency: u.cost_currency.or(Some(only_bucket)),
            price_currency: u.price_currency.or(Some(only_bucket)),
        };
        groups.insert(only_bucket, inferred);
    }

    // infer all other unknown postings from account inference
    let mut errors = Vec::<parser::AnnotatedError>::default();
    for u in unknown {
        let u_account = u.posting.account().item().as_ref();
        let inferred = CurrenciedPosting {
            posting: u.posting,
            units_currency: u
                .units_currency
                .or(account_currency_lookup.units(u_account)),
            cost_currency: u.cost_currency.or(account_currency_lookup.cost(u_account)),
            price_currency: u.price_currency,
        };
        if let Some(bucket) = inferred.bucket() {
            groups.insert(bucket, inferred);
        } else {
            errors.push(u.posting.error("failed to categorize posting").into())
        }
    }

    Ok(groups)
}
