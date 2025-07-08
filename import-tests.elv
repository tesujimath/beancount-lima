#!/usr/bin/env elvish

use os
use str

# all import tests currently run with the same config
set-env BEANCOUNT_LIMA_COGPATH (pwd)/tests/import.config:$E:BEANCOUNT_LIMA_COGPATH

for expected-path [tests/import/*[nomatch-ok].expected.beancount] {
  var import-dir = (str:replace .expected.beancount "" $expected-path)
  var ledger-path = (str:replace .expected "" $expected-path)
  var output-path = (str:replace .expected .actual $expected-path)
  var ledger-args = (if (os:exists $ledger-path) { put [--ledger $ledger-path ] } else { put [] })
  echo
  echo lima $@ledger-args import $import-dir/*
  lima $@ledger-args import $import-dir/* >$output-path
  diff -w $expected-path $output-path
}
