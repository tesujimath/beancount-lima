// TODO remove:
#![allow(dead_code, unused_variables)]
use beancount_parser_lima::ElementType;
use color_eyre::eyre::Result;
use rust_decimal::Decimal;
use steel::{
    gc::Shared,
    rvals::Custom,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
    SteelErr,
};
use steel_derive::Steel;
use time::Date;

use crate::types::{common::*, element::*, steel_date::SteelDate};

#[derive(Clone, Debug)]
pub(crate) struct Directive {
    pub(crate) date: Date,
    pub(crate) element: WrappedSpannedElement,
    pub(crate) variant: DirectiveVariant,
}

impl Custom for Directive {
    fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
        Some(Ok(self.to_string()))
    }
}

#[derive(Clone, Steel, Debug)]
pub(crate) enum DirectiveVariant {
    Transaction(Transaction),
    Price(Price),
    Balance(Balance),
    Open(Open),
    Close(Close),
    Commodity(Commodity),
    Pad(Pad),
    Document(Document),
    Note(Note),
    Event(Event),
    Query(Query),
}

// Scheme is not statically typed, so we don't force the user to unpack the directive variants, but rather support direct access
impl Directive {
    fn date(&self) -> SteelDate {
        self.date.into()
    }

    fn is_transaction(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Transaction(_))
    }

    fn as_transaction(&self) -> steel::rvals::Result<&Transaction> {
        if let DirectiveVariant::Transaction(x) = &self.variant {
            Ok(x)
        } else {
            Err(SteelErr::new(
                steel::rerrs::ErrorKind::TypeMismatch,
                format!(
                    "directive is a {} not a transaction",
                    self.element.element_type()
                ),
            ))
        }
    }

    fn transaction_postings(&self) -> steel::rvals::Result<Vec<Posting>> {
        Ok((*self.as_transaction()?.postings).clone())
    }

    fn transaction_flag(&self) -> steel::rvals::Result<String> {
        Ok(self.as_transaction()?.flag.clone())
    }

    fn transaction_payee(&self) -> steel::rvals::Result<Option<String>> {
        Ok(self.as_transaction()?.payee.clone())
    }

    fn transaction_narration(&self) -> steel::rvals::Result<Option<String>> {
        Ok(self.as_transaction()?.narration.clone())
    }

    fn is_price(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Price(_))
    }
    fn is_balance(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Balance(_))
    }
    fn is_open(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Open(_))
    }
    fn is_close(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Close(_))
    }
    fn is_commodity(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Commodity(_))
    }
    fn is_pad(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Pad(_))
    }
    fn is_document(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Document(_))
    }
    fn is_note(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Note(_))
    }
    fn is_event(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Event(_))
    }
    fn is_query(&self) -> bool {
        matches!(self.variant, DirectiveVariant::Query(_))
    }
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Transaction {
    pub(crate) flag: String,
    pub(crate) payee: Option<String>,
    pub(crate) narration: Option<String>,
    pub(crate) postings: Shared<Vec<Posting>>,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Price {}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Balance {
    pub(crate) account: String,
    pub(crate) amount: Amount,
    pub(crate) tolerance: Option<Decimal>,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Open {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Close {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Commodity {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Pad {
    pub(crate) source: String,
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Document {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Note {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Event {
    // TODO
}

#[derive(Clone, Steel, Debug)]
pub(crate) struct Query {
    // TODO
}

pub(crate) fn register_types(steel_engine: &mut Engine) {
    steel_engine.register_type::<Directive>("directive?");
    steel_engine.register_fn("directive-date", Directive::date);
    steel_engine.register_fn("transaction?", Directive::is_transaction);
    steel_engine.register_fn("price?", Directive::is_price);
    steel_engine.register_fn("balance?", Directive::is_balance);
    steel_engine.register_fn("open?", Directive::is_open);
    steel_engine.register_fn("close?", Directive::is_close);
    steel_engine.register_fn("commodity?", Directive::is_commodity);
    steel_engine.register_fn("pad?", Directive::is_pad);
    steel_engine.register_fn("document?", Directive::is_document);
    steel_engine.register_fn("note?", Directive::is_note);
    steel_engine.register_fn("event?", Directive::is_event);
    steel_engine.register_fn("query?", Directive::is_query);

    steel_engine.register_fn("transaction-postings", Directive::transaction_postings);
    steel_engine.register_fn("transaction-flag", Directive::transaction_flag);
    steel_engine.register_fn("transaction-payee", Directive::transaction_payee);
    steel_engine.register_fn("transaction-narration", Directive::transaction_narration);
}
