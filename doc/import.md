# Lima Import

The `lima import` command provides the following features:
- import from CSV or OFX v1 into Beancount format
- lookup primary account for import from OFX `acctid` field
- infer secondary accounts for postings from payees and narrations in existing Beancount ledger
- construct transaction ID from OFX `acctid` anf `fitid` fields, and reject import of duplicate transactions
- pair up transactions between accounts where both accounts are imported in the same group

The intention is that OFX v1 import is complete and general purpose.

CSV import, however, requires customising for each financial instituion according to the headers they export in CSV.

## Transaction IDs

A transaction ID is allocated to each transaction by the OFX v1 importer, and this is used to avoid re-importing the same transactions subsequently.  The ID is written to the metadata value `txnid`, (the key is configurable).

The transaction ID comprises `acctid.fitid`.

## Transaction pairing

When money is moved between accounts, the import file for each account contains a record of the transaction, which leads to a duplicate transaction traditionally requiring manual elimination.

`lima import` has heuristics to pair up transactions which are imported in the same group, removing the need for this manual step.

This requires secondary account inference to have allocated a single candidate account.

Pairing is performed only where the source and destination accounts and the value match, and the date is within some configurable threshold (default 3 days).
The result is a single transaction with both `txnid` and `txnid2` metadata values, or a comment in the case of import files missing transaction IDs. The payee and narration from the second transaction are also preserved as `payee2` and `narration2` metadata fields.  These fields are used for account inference in subsequent imports.
