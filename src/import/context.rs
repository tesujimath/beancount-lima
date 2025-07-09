use beancount_parser_lima::{
    self as parser, BeancountParser, BeancountSources, ParseError, ParseSuccess, Spanned,
};
use color_eyre::eyre::{eyre, Result};
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    path::Path,
};
use steel_derive::Steel;

// context for import, i.e. ledger
#[derive(Clone, Default, Debug, Steel)]
pub(crate) struct Context {
    pub(crate) path: String,
    pub(crate) txnids: HashSet<String>,
    pub(crate) payees: HashMap<String, HashMap<String, isize>>,
    pub(crate) narrations: HashMap<String, HashMap<String, isize>>,
}

#[derive(Default, Debug)]
struct ImportContextBuilder<'a> {
    path: String,
    txnid_keys: Vec<String>,
    payee2_key: String,
    narration2_key: String,
    txnids: HashSet<String>,
    payees: hashbrown::HashMap<&'a str, hashbrown::HashMap<&'a str, isize>>,
    narrations: hashbrown::HashMap<&'a str, hashbrown::HashMap<&'a str, isize>>,
    errors: Vec<parser::Error>,
}

impl Context {
    pub(crate) fn parse_from<W>(
        path: &Path,
        txnid_keys: Vec<String>,
        payee2_key: String,
        narration2_key: String,
        error_w: W,
    ) -> Result<Self>
    where
        W: Write + Copy,
    {
        let sources = BeancountSources::try_from(path)?;
        let parser = BeancountParser::new(&sources);

        match parser.parse() {
            Ok(ParseSuccess {
                directives,
                options: _,
                plugins: _,
                warnings,
            }) => {
                sources.write(error_w, warnings)?;
                let mut builder = ImportContextBuilder::new(
                    path.to_string_lossy().into_owned(),
                    txnid_keys,
                    payee2_key,
                    narration2_key,
                );

                for directive in &directives {
                    builder.directive(directive);
                }

                let result = builder.build(&sources, error_w);

                drop(directives);
                drop(parser);

                result
            }

            Err(ParseError { errors, warnings }) => {
                sources.write(error_w, errors)?;
                sources.write(error_w, warnings)?;
                Err(eyre!("parse error"))
            }
        }
    }

    pub(crate) fn path(&self) -> String {
        self.path.clone()
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
    fn new(
        path: String,
        txnid_keys: Vec<String>,
        payee2_key: String,
        narration2_key: String,
    ) -> Self {
        Self {
            path,
            txnid_keys,
            payee2_key,
            narration2_key,
            txnids: HashSet::default(),
            payees: hashbrown::HashMap::default(),
            narrations: hashbrown::HashMap::default(),
            errors: Vec::default(),
        }
    }

    fn build<W>(self, sources: &BeancountSources, error_w: W) -> Result<Context>
    where
        W: Write + Copy,
    {
        if self.errors.is_empty() {
            Ok(Context {
                path: self.path,
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
            sources.write(error_w, self.errors)?;
            Err(eyre!("builder error"))
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
                if let parser::MetaValue::Simple(parser::SimpleValue::String(txnid)) = txnid.item()
                {
                    if !self.txnids.contains(*txnid) {
                        self.txnids.insert(txnid.to_string());
                    }
                }
            }
        }

        // count primary account if we have payee2 or narration2 metadata
        let primary_account = transaction
            .postings()
            .next()
            .map(|p| p.account().item().as_ref());

        if let Some(payee2) = directive
            .metadata()
            .key_value(parser::Key::try_from(self.payee2_key.as_str()).unwrap())
        {
            if let parser::MetaValue::Simple(parser::SimpleValue::String(payee2)) = *payee2.item() {
                {
                    // ugh, borrow checker can't cope, so leak the string
                    count_accounts(
                        &mut self.payees,
                        payee2.to_string().leak(),
                        primary_account.iter().copied(),
                    );
                }
            }
        }

        if let Some(narration2) = directive
            .metadata()
            .key_value(parser::Key::try_from(self.narration2_key.as_str()).unwrap())
        {
            if let parser::MetaValue::Simple(parser::SimpleValue::String(narration2)) =
                *narration2.item()
            {
                {
                    // ugh, borrow checker can't cope, so leak the string
                    count_accounts(
                        &mut self.narrations,
                        narration2.to_string().leak(),
                        primary_account.iter().copied(),
                    );
                }
            }
        }

        // update payee and narration map to account name only for second and subsequent postings,
        // as the first posting is assumed to be the primary account
        match (transaction.payee(), transaction.narration()) {
            (None, None) => (),
            (payee, narration) => {
                let accounts = transaction
                    .postings()
                    .skip(1) // skip first posting, assumed to be for primary account
                    .map(|p| p.account().item().as_ref())
                    .collect::<Vec<&str>>();

                if let Some(payee) = payee {
                    count_accounts(&mut self.payees, payee.item(), accounts.iter().copied());
                }

                if let Some(narration) = narration {
                    count_accounts(
                        &mut self.narrations,
                        narration.item(),
                        accounts.iter().copied(),
                    );
                }
            }
        }
    }
}

/// Accumulate the counts for the inferred accounts
fn count_accounts<'a, I>(
    buckets: &mut hashbrown::HashMap<&'a str, hashbrown::HashMap<&'a str, isize>>,
    key: &'a str,
    accounts: I,
) where
    I: Iterator<Item = &'a str>,
{
    use hashbrown::hash_map::Entry::*;
    match buckets.entry(key) {
        Occupied(mut payees) => {
            let payees = payees.get_mut();
            for account in accounts {
                match payees.entry(account) {
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
                    .map(|account| (account, 1))
                    .collect::<hashbrown::HashMap<_, _>>(),
            );
        }
    }
}
