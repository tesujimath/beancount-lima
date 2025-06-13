use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Spanned,
};
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::Path,
};
use steel_derive::Steel;

use crate::Error;

// context for import, i.e. ledger
#[derive(Clone, Default, Debug, Steel)]
pub(crate) struct Context {
    pub(crate) txnids: HashSet<String>,
    pub(crate) payees: HashMap<String, HashMap<String, isize>>,
    pub(crate) narrations: HashMap<String, HashMap<String, isize>>,
}

#[derive(Default, Debug)]
struct ImportContextBuilder<'a> {
    txnid_keys: Vec<String>,
    txnids: HashSet<String>,
    payees: hashbrown::HashMap<&'a str, hashbrown::HashMap<&'a str, isize>>,
    narrations: hashbrown::HashMap<&'a str, hashbrown::HashMap<&'a str, isize>>,
    errors: Vec<parser::Error>,
}

impl Context {
    pub(crate) fn parse_from<W>(
        path: &Path,
        txnid_keys: Vec<String>,
        error_w: W,
    ) -> Result<Self, Error>
    where
        W: Write + Copy,
    {
        let sources = BeancountSources::try_from(path).map_err(Error::Io)?;
        let parser = BeancountParser::new(&sources);

        match parser.parse() {
            Ok(ParseSuccess {
                directives,
                options: _,
                plugins: _,
                warnings,
            }) => {
                sources.write(error_w, warnings).map_err(Error::Io)?;
                let mut builder = ImportContextBuilder::new(txnid_keys);

                for directive in &directives {
                    builder.directive(directive);
                }

                let result = builder.build(&sources, error_w);

                drop(directives);
                drop(parser);

                result
            }

            Err(ParseError { errors, warnings }) => {
                sources.write(error_w, errors).map_err(Error::Io)?;
                sources.write(error_w, warnings).map_err(Error::Io)?;
                Err(Error::Parser)
            }
        }
    }

    pub(crate) fn txnids(&self) -> Vec<String> {
        self.txnids.iter().cloned().collect::<Vec<_>>()
    }

    pub(crate) fn payees(&self) -> HashMap<String, HashMap<String, isize>> {
        self.payees.clone()
    }

    pub(crate) fn narrations(&self) -> HashMap<String, HashMap<String, isize>> {
        self.narrations.clone()
    }
}

impl<'a> ImportContextBuilder<'a> {
    fn new(txnid_keys: Vec<String>) -> Self {
        Self {
            txnid_keys,
            txnids: HashSet::default(),
            payees: hashbrown::HashMap::default(),
            narrations: hashbrown::HashMap::default(),
            errors: Vec::default(),
        }
    }

    fn build<W>(self, sources: &BeancountSources, error_w: W) -> Result<Context, Error>
    where
        W: Write + Copy,
    {
        if self.errors.is_empty() {
            Ok(Context {
                txnids: self.txnids,
                payees: self
                    .payees
                    .into_iter()
                    .map(|(name, accounts)| {
                        (
                            name.to_string(),
                            accounts
                                .into_iter()
                                .map(|(k, v)| (k.to_string(), v))
                                .collect(),
                        )
                    })
                    .collect(),
                narrations: self
                    .narrations
                    .into_iter()
                    .map(|(name, accounts)| {
                        (
                            name.to_string(),
                            accounts
                                .into_iter()
                                .map(|(k, v)| (k.to_string(), v))
                                .collect(),
                        )
                    })
                    .collect(),
            })
        } else {
            sources.write(error_w, self.errors).map_err(Error::Io)?;
            Err(Error::Builder)
        }
    }

    fn directive(&mut self, directive: &'a Spanned<parser::Directive<'a>>) {
        use parser::DirectiveVariant::*;

        if let Transaction(transaction) = directive.variant() {
            self.transaction(transaction, directive)
        }
    }

    fn transaction<'s, 'b>(
        &'s mut self,
        transaction: &'b parser::Transaction,
        directive: &'b Spanned<parser::Directive>,
    ) where
        'b: 's,
        'b: 'a,
    {
        // record transaction ID if it exists in the metadata
        for txnid_key in self.txnid_keys.iter() {
            if let Some(txnid) = directive
                .metadata()
                .key_value(parser::Key::try_from(txnid_key.as_str()).unwrap())
            {
                if let parser::MetaValue::Simple(parser::SimpleValue::String(s)) = txnid.item() {
                    if !self.txnids.contains(*s) {
                        self.txnids.insert(s.to_string());
                    }
                }
            }
        }

        // update payee and narration map to account name only for second and subsequent postings,
        // as the first posting is assumed to be the primary account
        use hashbrown::hash_map::Entry::*;
        match (transaction.payee(), transaction.narration()) {
            (None, None) => (),
            (payee, narration) => {
                let accounts = transaction
                    .postings()
                    .skip(1) // skip first posting, assumed to be for primary account
                    .map(|p| p.account().item().as_ref())
                    .collect::<Vec<&str>>();

                if let Some(payee) = payee {
                    match self.payees.entry(payee.item()) {
                        Occupied(mut payees) => {
                            let payees = payees.get_mut();
                            for account in accounts.iter() {
                                match payees.entry(*account) {
                                    Occupied(mut payee_account) => {
                                        let payee_account = payee_account.get_mut();
                                        *payee_account += 1;
                                    }
                                    Vacant(payee_account) => {
                                        payee_account.insert(1);
                                    }
                                }
                            }
                        }
                        Vacant(payees) => {
                            payees.insert(
                                accounts
                                    .iter()
                                    .map(|account| (*account, 1))
                                    .collect::<hashbrown::HashMap<_, _>>(),
                            );
                        }
                    }
                }

                if let Some(narration) = narration {
                    match self.narrations.entry(narration.item()) {
                        Occupied(mut narrations) => {
                            let narrations = narrations.get_mut();
                            for account in accounts.iter() {
                                match narrations.entry(*account) {
                                    Occupied(mut narration_account) => {
                                        let narration_account = narration_account.get_mut();
                                        *narration_account += 1;
                                    }
                                    Vacant(narration_account) => {
                                        narration_account.insert(1);
                                    }
                                }
                            }
                        }
                        Vacant(narrations) => {
                            narrations.insert(
                                accounts
                                    .iter()
                                    .map(|account| (*account, 1))
                                    .collect::<hashbrown::HashMap<_, _>>(),
                            );
                        }
                    }
                }
            }
        }
    }
}
