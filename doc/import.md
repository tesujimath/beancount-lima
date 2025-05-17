# Lima Import

The `lima import` command provides the following features:
- import from CSV or OFX v1 into Beancount format
- lookup primary account for import from OFX `acctid` field
- infer secondary accounts for postings from payees and narrations in existing Beancount ledger
- construct transaction ID from OFX `acctid` anf `fitid` fields, and reject import of duplicate transactions

The intention is that OFX v1 import is complete and general purpose.

CSV import, however, requires customising for each financial instituion according to the headers they export in CSV.

## Transaction IDs

A transaction ID is allocated to each transaction by the OFX v1 importer, and this is used to avoid re-importing the same transactions subsequently.  The ID is written to the metadata value `txnid`, (the key is configurable).

The transaction ID comprises `acctid.fitid`.
