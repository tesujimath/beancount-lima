use rust_decimal::Decimal;
use std::result::Result;
use steel::{
    gc::GcMut,
    rvals::{as_underlying_type, CustomType},
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr, SteelVal,
};
use tabulator::{Align, Cell, Gap};

use crate::types::{Amount, SteelDecimal};

fn custom_to_cell<'a>(value: &GcMut<Box<dyn CustomType>>) -> Result<Cell<'a>, SteelErr> {
    use Align::Left;

    let value = value.read();

    if let Some(value) = as_underlying_type::<SteelDecimal>(value.as_ref()) {
        Ok(Into::<Decimal>::into(*value).into())
    } else if let Some(value) = as_underlying_type::<Amount>(value.as_ref()) {
        Ok(value.clone().into())
    } else {
        value.display().map(|s| (s, Left).into()).map_err(|e| {
            SteelErr::new(
                steel::rerrs::ErrorKind::TypeMismatch,
                format!("failed to format custom value: {e}"),
            )
        })
    }
}

fn container_to_cell<'a, I>(value: I, gap: Gap) -> Result<Cell<'a>, SteelErr>
where
    I: IntoIterator<Item = &'a SteelVal>,
{
    value
        .into_iter()
        .map(steelval_to_cell)
        .collect::<Result<Vec<_>, SteelErr>>()
        .map(|cells| Cell::Row(cells, gap))
}

fn anchored_rational(s: String) -> Cell<'static> {
    let anchor = s
        .find('/')
        .map(|idx| idx - 1)
        .unwrap_or_else(|| s.len() - 1);
    Cell::anchored(s, anchor)
}

fn steelval_to_cell(value: &SteelVal) -> Result<Cell, SteelErr> {
    use Align::Left;
    use SteelVal::*;

    match value {
        BoolV(x) => Ok((x.to_string(), Left).into()),
        NumV(x) => Ok((*x).into()),
        IntV(x) => Ok((*x).into()),
        BigNum(x) => Ok(x.as_ref().into()),
        Rational(x) => Ok(anchored_rational(x.to_string())),
        BigRational(x) => Ok(anchored_rational(x.to_string())),
        CharV(x) => Ok((x.to_string(), Left).into()),
        StringV(s) => Ok((s.as_str(), Left).into()),
        SymbolV(s) => Ok((s.as_str(), Left).into()),
        Complex(x) => Ok((x.to_string(), Left).into()),

        Custom(x) => custom_to_cell(x),

        ListV(x) => container_to_cell(x, Gap::default()),

        // TODO turn these into rows
        // VectorV(x) => Ok(Cell::Left(Borrowed("oops"))),
        // IterV(x) => Ok(Cell::Left(Borrowed("oops"))),
        // Pair(x) => Ok(Cell::Left(Borrowed("oops"))),
        // MutableVector(x) => Ok(Cell::Left(Borrowed("oops"))),
        // BoxedIterator(x) => Ok(Cell::Left(Borrowed("oops"))),
        // Boxed(x) => Ok(Cell::Left(Borrowed("oops"))),
        // HeapAllocated(x) => Ok(Cell::Left(Borrowed("oops"))),
        _ => Err(SteelErr::new(
            steel::rerrs::ErrorKind::TypeMismatch,
            format!("can't tabulate {value:?}"),
        )),
    }
}

fn table_to_cell(rows: &[Vec<SteelVal>]) -> Result<Cell, SteelErr> {
    rows.iter()
        .map(|row| {
            row.iter()
                .map(steelval_to_cell)
                .collect::<Result<Vec<_>, SteelErr>>()
                .map(|cells| Cell::Row(cells, Gap::default()))
        })
        .collect::<Result<Vec<_>, SteelErr>>()
        .map(Cell::Column)
}

fn tabulate(rows: Vec<Vec<SteelVal>>) -> Result<String, SteelErr> {
    table_to_cell(&rows).map(|cell| cell.to_string())
}

pub(crate) fn register(steel_engine: &mut Engine) {
    steel_engine.register_fn("tabulate", tabulate);
}
