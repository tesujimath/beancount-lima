use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use std::collections::HashMap;

use crate::types::WrappedSpannedElement;

#[derive(Debug)]
pub(crate) struct InferredTolerance<'a> {
    fallback: Option<Decimal>,
    by_currency: HashMap<parser::Currency<'a>, Decimal>,

    multiplier: Decimal,
}

impl<'a> InferredTolerance<'a> {
    pub(crate) fn new(options: &'a parser::Options<'a>) -> Self {
        Self {
            fallback: options.inferred_tolerance_default_fallback(),
            by_currency: options
                .inferred_tolerance_defaults()
                .filter_map(|(cur, value)| cur.map(|cur| (cur, value)))
                .collect::<HashMap<_, _>>(),
            multiplier: options.inferred_tolerance_multiplier(),
        }
    }
}

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

pub(crate) fn balance_transaction<'a>(
    transaction: &'a parser::Transaction<'a>,
    inferred_tolerance: &InferredTolerance<'a>,
    element: &WrappedSpannedElement,
) -> Result<Vec<Weight<'a>>, parser::AnnotatedError> {
    tracing::debug!("transaction postings");
    let postings = transaction.postings().collect::<Vec<_>>();
    for posting in &postings {
        tracing::debug!("{:?}", posting.item());
    }

    // determine initial weights which may have multiple gaps, but must be uniquely fillable
    let mut weights = transaction
        .postings()
        .map(|posting| {
            TryInto::<WeightBuilder<'_>>::try_into(posting.item())
                .map_err(|e| posting.error(e).into())
        })
        .collect::<Result<Vec<WeightBuilder<'_>>, parser::AnnotatedError>>()?;
    tracing::debug!("initial weights {:?}", &weights);

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

    let mut currency_balance =
        ignore_tolerable_residuals(&weights, currency_balance, inferred_tolerance);
    tracing::debug!(
        "currency balance ignoring tolerable residuals {:?}",
        &currency_balance
    );

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

    tracing::debug!(
        "after phase 1 weights are {:?}, currency_balance {:?}",
        &weights,
        &currency_balance
    );

    // all that can remain now are the weights with unspecified currency, at most one of which may have an unspecified number
    for (i, w) in weights.iter_mut().enumerate() {
        if let (Some(num_w), None) = (w.number, w.currency) {
            if currency_balance.len() == 1 {
                let (cur, mut num) = currency_balance.drain().next().unwrap();
                w.currency = Some(cur);
                num += num_w;
                if !num.is_zero() {
                    currency_balance.insert(cur, num);
                }
            } else {
                return Err(postings[i]
                    .error(format!("can't infer currency{}", w.source.reason()))
                    .into());
            }
        }
    }

    tracing::debug!(
        "after phase 2 weights are {:?}, currency_balance {:?} len {}",
        &weights,
        &currency_balance,
        currency_balance.len()
    );

    for (i, w) in weights.iter_mut().enumerate() {
        if let (None, None) = (w.number, w.currency) {
            if currency_balance.len() == 1 {
                let (cur, num) = currency_balance.drain().next().unwrap();
                w.currency = Some(cur);
                w.number = Some(-num);
                tracing::debug!("allocated weight {i} {:?}", &w);
            } else {
                return Err(postings[i]
                    .error(format!("can't infer anything{}", w.source.reason()))
                    .into());
            }
        }
    }

    if !currency_balance.is_empty() {
        // sort by currency so deterministic
        let mut currencies = currency_balance.keys().copied().collect::<Vec<_>>();
        currencies.sort();
        return Err(element.error(format!(
            "balancing error {}",
            currencies
                .into_iter()
                .map(|cur| format!("{} {}", currency_balance.get(cur).unwrap(), cur))
                .collect::<Vec<String>>()
                .join(", ")
        )));
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

    tracing::debug!("fully specified {:?}", &fully_specified);

    Ok(fully_specified)
}

fn ignore_tolerable_residuals<'a>(
    weights: &[WeightBuilder],
    residual: hashbrown::HashMap<&'a parser::Currency<'a>, rust_decimal::Decimal>,
    inferred_tolerance: &InferredTolerance<'a>,
) -> hashbrown::HashMap<&'a parser::Currency<'a>, rust_decimal::Decimal> {
    use hashbrown::hash_map::Entry::*;

    // determine coarsest scale for each currency according to the weights we have accumulated
    let mut coarsest_scale_for_tolerance = hashbrown::HashMap::<&parser::Currency, u32>::default();
    for w in weights {
        if let (Some(cur), Some(num)) = (w.currency, w.number) {
            match coarsest_scale_for_tolerance.entry(cur) {
                Occupied(mut entry) => {
                    let coarsest_so_far = entry.get_mut();
                    if num.scale() < *coarsest_so_far {
                        *coarsest_so_far = num.scale();
                    }
                }
                Vacant(entry) => {
                    entry.insert(num.scale());
                }
            }
        }
    }

    // remove all but intolerable residuals
    residual
        .into_iter()
        .filter_map(|(cur, num)| {
            let abs_num = num.abs();

            match coarsest_scale_for_tolerance.get(cur) {
                coarsest_scale @ (Some(&0) | None) => {
                    if coarsest_scale.is_some() {
                        tracing::debug!(
                            "no tolerance for residual {:?} for integer currency {:?}",
                            abs_num,
                            cur
                        );
                    } else {
                        tracing::debug!(
                            "can't infer local tolerance for residual {:?} for currency {:?}",
                            abs_num,
                            cur
                        );
                    }
                    if let Some(tol) = inferred_tolerance
                        .by_currency
                        .get(cur)
                        .or(inferred_tolerance.fallback.as_ref())
                    {
                        let intolerable = &abs_num > tol;

                        tracing::debug!(
                            "tolerance {} for {:?} {:?} against {:?}",
                            if intolerable { "exceeded" } else { "ok" },
                            abs_num,
                            cur,
                            tol,
                        );

                        intolerable.then_some((cur, num))
                    } else {
                        tracing::debug!(
                            "no tolerance for residual {:?} for currency {:?}",
                            abs_num,
                            cur
                        );
                        (abs_num > Decimal::ZERO).then_some((cur, num))
                    }
                }

                Some(coarsest_scale) => {
                    let unit = Decimal::new(1, *coarsest_scale);
                    let tol = unit * inferred_tolerance.multiplier;
                    let intolerable = abs_num > tol;

                    tracing::debug!(
                    "tolerance {} for {:?} {:?} against unit = {:?}, multiplier = {:?}, tol = {:?}",
                    if intolerable { "exceeded" } else { "ok" },
                    abs_num,
                    cur,
                    unit,
                    inferred_tolerance.multiplier,
                    tol,
                );
                    intolerable.then_some((cur, num))
                }
            }
        })
        .collect::<hashbrown::HashMap<&parser::Currency<'a>, Decimal>>()
}

#[cfg(test)]
mod tests;
