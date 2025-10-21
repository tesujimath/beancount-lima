use beancount_parser_lima as parser;
use rust_decimal::Decimal;
use time::Date;

#[derive(Clone, Debug)]
enum Directive<'a> {
    Raw(parser::Directive<'a>),
    Transaction(Transaction<'a>),
}

#[derive(Clone, Debug)]
pub(crate) struct Transaction<'a> {
    pub(crate) parser: parser::Transaction<'a>,
    pub(crate) postings: Vec<Posting<'a>>,
}

#[derive(Clone, Debug)]
pub(crate) struct Posting<'a> {
    pub(crate) parser: Option<parser::Posting<'a>>,
    pub(crate) amount: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) cost: Option<Cost<'a>>,
    pub(crate) price: Option<Price<'a>>,
    // pub(crate) metadata: Metadata<'a>,
}

#[derive(Clone, Debug)]
pub(crate) struct Cost<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
    pub(crate) date: Date,
    pub(crate) label: Option<&'a str>,
    pub(crate) merge: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct Price<'a> {
    pub(crate) per_unit: Decimal,
    pub(crate) currency: &'a parser::Currency<'a>,
}
