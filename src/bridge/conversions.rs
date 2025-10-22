use crate::loader::{self as loader, into_spanned_element};
use crate::prism::{self as prism};

pub(crate) fn convert_directives(
    loaded_directives: Vec<loader::Directive>,
) -> Vec<prism::Directive> {
    use beancount_parser_lima::DirectiveVariant as PDV;
    use loader::DirectiveVariant as LDV;
    let mut directives = Vec::default();

    for directive in loaded_directives {
        let date = *directive.parsed.date().item();
        let element = into_spanned_element(directive.parsed).into();
        match directive.parsed.variant() {
            PDV::Transaction(parsed_transaction) => {
                if let LDV::Transaction(loaded) = directive.loaded {
                    directives.push(prism::Directive {
                        date,
                        element,
                        variant: prism::DirectiveVariant::Transaction(prism::Transaction {
                            postings: loaded
                                .postings
                                .into_iter()
                                .map(Into::<prism::Posting>::into)
                                .collect::<Vec<_>>()
                                .into(),
                            flag: parsed_transaction.flag().to_string(),
                            payee: parsed_transaction.payee().map(|payee| payee.to_string()),
                            narration: parsed_transaction
                                .narration()
                                .map(|narration| narration.to_string()),
                        }),
                    });
                } else {
                    panic!(
                        "mismatch between variants parsed transaction {:?}, loaded {:?}",
                        &parsed_transaction, &directive.loaded
                    );
                }
            }
            PDV::Price(price) => directives.push(prism::Directive {
                date,
                element,
                variant: prism::DirectiveVariant::Price(prism::Price {
                    currency: price.currency().item().to_string(),
                    amount: price.amount().item().into(),
                }),
            }),
            PDV::Balance(balance) => {
                let balance = prism::Balance {
                    account: balance.account().item().to_string(),
                    amount: (
                        balance.atol().amount().number().value(),
                        balance.atol().amount().currency(),
                    )
                        .into(),
                    tolerance: balance
                        .atol()
                        .tolerance()
                        .map(|tolerance| (*tolerance.item())),
                };

                directives.push(prism::Directive {
                    date,
                    element,
                    variant: prism::DirectiveVariant::Balance(balance),
                });
            }
            PDV::Open(open) => {
                let mut currencies = open
                    .currencies()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>();
                currencies.sort();

                directives.push(prism::Directive {
                    date,
                    element,
                    variant: prism::DirectiveVariant::Open(prism::Open {
                        account: open.account().item().to_string(),
                        currencies,
                        booking: open.booking().map(|booking| (*booking.item()).into()),
                    }),
                });
            }
            PDV::Close(close) => directives.push(prism::Directive {
                date,
                element,
                variant: prism::DirectiveVariant::Close(prism::Close {
                    account: close.account().item().to_string(),
                }),
            }),
            PDV::Commodity(commodity) => directives.push(prism::Directive {
                date,
                element,
                variant: prism::DirectiveVariant::Commodity(prism::Commodity {
                    currency: commodity.currency().item().to_string(),
                }),
            }),
            PDV::Pad(parsed_pad) => {
                if let LDV::Pad(loaded) = directive.loaded {
                    directives.push(prism::Directive {
                        date,
                        element: element.clone(),
                        variant: prism::DirectiveVariant::Pad(prism::Pad {
                            account: parsed_pad.account().item().to_string(),
                            source: parsed_pad.source().to_string(),
                        }),
                    });

                    directives.push(prism::Directive {
                        date,
                        element,
                        variant: prism::DirectiveVariant::Transaction(prism::Transaction {
                            postings: loaded
                                .postings
                                .into_iter()
                                .map(Into::<prism::Posting>::into)
                                .collect::<Vec<_>>()
                                .into(),
                            flag: crate::loader::PAD_FLAG.into(),
                            payee: None,
                            narration: None,
                        }),
                    });
                } else {
                    panic!(
                        "mismatch between variants parsed pad {:?}, loaded {:?}",
                        &parsed_pad, &directive.loaded
                    );
                }
            }
            PDV::Document(document) => directives.push(prism::Directive {
                date,
                element: element.clone(),
                variant: prism::DirectiveVariant::Document(prism::Document {
                    account: document.account().item().to_string(),
                    path: document.path().item().to_string(),
                }),
            }),
            PDV::Note(note) => directives.push(prism::Directive {
                date,
                element: element.clone(),
                variant: prism::DirectiveVariant::Note(prism::Note {
                    account: note.account().item().to_string(),
                    comment: note.comment().item().to_string(),
                }),
            }),
            PDV::Event(event) => directives.push(prism::Directive {
                date,
                element: element.clone(),
                variant: prism::DirectiveVariant::Event(prism::Event {
                    event_type: event.event_type().item().to_string(),
                    description: event.description().item().to_string(),
                }),
            }),
            PDV::Query(query) => directives.push(prism::Directive {
                date,
                element: element.clone(),
                variant: prism::DirectiveVariant::Query(prism::Query {
                    name: query.name().item().to_string(),
                    content: query.content().item().to_string(),
                }),
            }),
        }
    }
    directives
}

impl From<loader::Posting<'_>> for prism::Posting {
    fn from(value: loader::Posting<'_>) -> Self {
        prism::Posting::new(
            value.account,
            (value.amount, value.currency.to_string()).into(),
            value.flag,
            value.cost.map(Into::<prism::Cost>::into),
        )
    }
}

impl From<loader::Cost<'_>> for prism::Cost {
    fn from(value: loader::Cost<'_>) -> Self {
        Self {
            per_unit: value.per_unit,
            currency: value.currency.to_string(),
            date: value.date,
            label: value.label.map(ToString::to_string),
            merge: value.merge,
        }
    }
}
