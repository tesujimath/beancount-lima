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

## Diagnostics for mismatched balances

A bother when importing transactions is finding that the asserted balance is different from the sum of the postings.

To facilitate resolving such problems, when a balance mismatch is detected, all posts to that account since the
most recent balance assertion are printed along with the error message.

For example:
```
aya> lima --ledger examples/beancount/balance.beancount
Error: invalid balance
    ╭─[ examples/beancount/balance.beancount:27:1 ]
    │
 27 │ 2020-02-28 balance Assets:Bank:Current   940.00 NZD
    │ ─────────────────────────┬─────────────────────────
    │                          ╰─────────────────────────── accumulated 935.00 NZD, error 5.00 NZD
────╯
2020-01-28              1000.00 NZD
2020-02-01  -50.00 NZD   950.00 NZD  Food
2020-02-02  -15.00 NZD   935.00 NZD  Drinks
```

## Standlone mode

When run as `lima -o standalone import` the reference ledger is `include`d in the output, so that it stands alone.
