use beancount_parser_lima as parser;

use crate::types::WrappedSpannedElement;

// Calculate weight of postings and balance transaction as per:
//
// https://beancount.github.io/docs/beancount_language_syntax.html#balancing-rule-the-weight-of-postings

#[derive(Clone, Debug)]
struct WeightBuilder<'a> {
    number: Option<rust_decimal::Decimal>,
    currency: Option<&'a parser::Currency<'a>>,
    source: WeightSource,
}

#[derive(Clone, Debug)]
pub(crate) struct Weight<'a> {
    pub(crate) number: rust_decimal::Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) source: WeightSource,
}

impl<'a> Weight<'a> {
    fn new(
        number: rust_decimal::Decimal,
        currency: &'a parser::Currency<'a>,
        source: WeightSource,
    ) -> Self {
        Self {
            number,
            currency,
            source,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum WeightSource {
    Native,
    Cost,
    Price,
}

impl WeightSource {
    fn reason(&self) -> &'static str {
        use WeightSource::*;

        match self {
            Native => "",
            Cost => "for cost",
            Price => "for price",
        }
    }
}

impl<'a> WeightBuilder<'a> {
    fn new(
        number: Option<rust_decimal::Decimal>,
        currency: Option<&'a parser::Currency<'a>>,
        source: WeightSource,
    ) -> Self {
        Self {
            number,
            currency,
            source,
        }
    }
}

impl<'a>
    TryFrom<(
        &'a parser::ScopedExprValue,
        Option<&'a parser::Currency<'a>>,
        Option<rust_decimal::Decimal>,
    )> for WeightBuilder<'a>
{
    type Error = &'static str;

    fn try_from(
        value: (
            &'a parser::ScopedExprValue,
            Option<&'a parser::Currency<'a>>,
            Option<rust_decimal::Decimal>,
        ),
    ) -> Result<Self, Self::Error> {
        use parser::ScopedExprValue::*;

        match value {
            (PerUnit(expr), currency, Some(units)) => Ok(WeightBuilder::new(
                Some(expr.value() * units),
                currency,
                WeightSource::Price,
            )),
            (PerUnit(_), _, None) => Err("can't have price per unit without units"),
            (Total(expr), currency, _) => Ok(WeightBuilder::new(
                Some(expr.value()),
                currency,
                WeightSource::Price,
            )),
        }
    }
}

impl<'a> TryFrom<(&'a parser::PriceSpec<'a>, Option<rust_decimal::Decimal>)> for WeightBuilder<'a> {
    type Error = &'static str;

    fn try_from(
        value: (&'a parser::PriceSpec<'a>, Option<rust_decimal::Decimal>),
    ) -> Result<Self, Self::Error> {
        use parser::PriceSpec::*;

        match value {
            (Unspecified, _) => Ok(WeightBuilder::new(None, None, WeightSource::Price)),
            (BareCurrency(currency), _) => Ok(WeightBuilder::new(
                None,
                Some(currency),
                WeightSource::Price,
            )),
            (BareAmount(expr), units) => (expr, None, units).try_into(),
            (CurrencyAmount(expr, currency), units) => (expr, Some(currency), units).try_into(),
        }
    }
}

impl<'a> TryFrom<(&'a parser::CostSpec<'a>, Option<rust_decimal::Decimal>)> for WeightBuilder<'a> {
    type Error = &'static str;

    fn try_from(
        value: (&'a parser::CostSpec<'a>, Option<rust_decimal::Decimal>),
    ) -> Result<Self, Self::Error> {
        match (
            value.0.per_unit().map(|per_unit| per_unit.item().value()),
            value.0.total().map(|total| total.item().value()),
            value.0.currency().map(|currency| currency.item()),
            value.1,
        ) {
            (_, Some(total), currency, _) => Ok(WeightBuilder::new(
                Some(total),
                currency,
                WeightSource::Cost,
            )),
            (Some(per_unit), _, currency, Some(units)) => Ok(WeightBuilder::new(
                Some(per_unit * units),
                currency,
                WeightSource::Cost,
            )),
            (Some(_), _, _, None) => Err("can't have cost per-unit without units"),
            (None, None, currency, _) => Ok(WeightBuilder::new(None, currency, WeightSource::Cost)),
        }
    }
}

impl<'a> TryFrom<&'a parser::Posting<'a>> for WeightBuilder<'a> {
    type Error = &'static str;

    fn try_from(value: &'a parser::Posting<'a>) -> Result<Self, Self::Error> {
        match (
            value.amount().map(|amount| amount.item().value()),
            value.currency().map(|currency| currency.item()),
            value.cost_spec().map(|cost_spec| cost_spec.item()),
            value
                .price_annotation()
                .map(|price_annotation| price_annotation.item()),
        ) {
            (units, currency, None, None) => {
                Ok(WeightBuilder::new(units, currency, WeightSource::Native))
            }
            (units, _, Some(cost_spec), _) => (cost_spec, units).try_into(),
            (units, _, None, Some(price_annotation)) => (price_annotation, units).try_into(),
        }
    }
}

pub(crate) fn determine_transaction_weight<'a>(
    transaction: &'a parser::Transaction<'a>,
    element: WrappedSpannedElement,
) -> Result<Vec<Weight<'a>>, parser::AnnotatedError> {
    println!("transaction postings");
    let postings = transaction.postings().collect::<Vec<_>>();
    for posting in &postings {
        println!("{:?}", posting.item());
    }

    // determine initial weights which may have multiple gaps, but must be uniquely fillable
    let mut weights = transaction
        .postings()
        .map(|posting| {
            TryInto::<WeightBuilder<'_>>::try_into(posting.item())
                .map_err(|e| posting.error(e).into())
        })
        .collect::<Result<Vec<WeightBuilder<'_>>, parser::AnnotatedError>>()?;
    println!("initial weights {:?}", &weights);

    let mut currency_balance =
        hashbrown::HashMap::<&parser::Currency, rust_decimal::Decimal>::default();
    // collect up balance per currency
    for w in &weights {
        use hashbrown::hash_map::Entry::*;
        if let (Some(num), Some(cur)) = (w.number, w.currency) {
            match currency_balance.entry(cur) {
                Occupied(mut balance) => {
                    let balance = balance.get_mut();
                    *balance += num;
                }
                Vacant(balance) => {
                    balance.insert(num);
                }
            }
        }
    }
    println!("currency balance {:?}", &currency_balance);

    // and use this to infill missing values in the weights
    for (i, w) in weights.iter_mut().enumerate() {
        if let (None, Some(cur)) = (w.number, w.currency) {
            if let Some(num) = currency_balance.remove(cur) {
                w.number = Some(-num);
            } else {
                return Err(postings[i]
                    .error(format!("can't infer amount of {cur}{}", w.source.reason()))
                    .into());
            }
        }
    }

    println!("after phase 1 weights are {:?}", &weights);

    // all that can remain now are the weights with unspecified currency, at most one of which may have an unspecified number
    let mut unused_currency_balance = currency_balance.drain().collect::<Vec<_>>();
    for (i, w) in weights.iter_mut().enumerate() {
        if let (Some(num_w), None) = (w.number, w.currency) {
            if unused_currency_balance.len() == 1 {
                let (cur, mut num) = unused_currency_balance.pop().unwrap();
                w.currency = Some(cur);
                num += num_w;
                if !num.is_zero() {
                    unused_currency_balance.push((cur, num));
                }
            } else {
                return Err(postings[i]
                    .error(format!("can't infer currency{}", w.source.reason()))
                    .into());
            }
        }
    }

    for (i, w) in weights.iter_mut().enumerate() {
        if let (None, None) = (w.number, w.currency) {
            if unused_currency_balance.len() == 1 {
                let (cur, num) = unused_currency_balance.pop().unwrap();
                w.currency = Some(cur);
                w.number = Some(-num);
            } else {
                return Err(postings[i]
                    .error(format!("can't infer anything{}", w.source.reason()))
                    .into());
            }
        }
    }

    // check there's nothing left unused
    if !unused_currency_balance.is_empty() {
        return Err(element.error(format!("surplus balance {:?}", &unused_currency_balance)));
    }

    // TODO remove this sanity check which can't fail
    if weights
        .iter()
        .any(|w| w.number.is_none() || w.currency.is_none())
    {
        panic!("unspecified weights remain {:?}", &weights);
    }

    let fully_specified = weights
        .into_iter()
        .map(|w| Weight::new(w.number.unwrap(), w.currency.unwrap(), w.source))
        .collect::<Vec<_>>();

    println!("fully specified {:?}", &fully_specified);

    Ok(fully_specified)
}

/// For balancing a transaction we use the price if there is one, otherwise simply the amount.
///
// TODO: infer the price from a partial spec.
pub(crate) fn get_posting_amount_for_balancing_transaction<'a, 'b>(
    posting: &'b parser::Posting<'a>,
) -> Option<(rust_decimal::Decimal, &'b parser::Currency<'a>)>
where
    'b: 'a,
{
    if let Some(price) = posting.price_annotation() {
        use parser::PriceSpec::*;
        use parser::ScopedExprValue::*;

        match price.item() {
            Unspecified => None,
            BareCurrency(_price_currency) => None,
            BareAmount(_price_amount) => None,
            CurrencyAmount(PerUnit(price_amount), price_currency) => posting
                .amount()
                .map(|amount| (amount.value() * price_amount.value(), price_currency)),
            CurrencyAmount(Total(price_amount), price_currency) => {
                Some((price_amount.value(), price_currency))
            }
        }
    } else if let (Some(amount), Some(currency)) = (posting.amount(), posting.currency()) {
        Some((amount.value(), currency.item()))
    } else {
        None
    }
}

#[cfg(test)]
mod tests;
