#!/usr/bin/env elvish

use os
use str

# all import tests currently run with the same config
set-env BEANCOUNT_LIMA_COGPATH (pwd)/tests/import.config:$E:BEANCOUNT_LIMA_COGPATH

for importer [(ls tests/import)] {
  echo
  echo importer $importer
  for expected-path [tests/import/$importer/*[nomatch-ok].expected.beancount] {
    var import-dir = (str:replace .expected.beancount "" $expected-path)
    var ledger-path = (str:replace .expected "" $expected-path)
    var output-path = (str:replace .expected .actual $expected-path)
    var ledger-args = (if (os:exists $ledger-path) { put [--ledger $ledger-path ] } else { put [] })
    echo lima --batch import --using $importer $@ledger-args $import-dir/*
    lima --batch import --using $importer $@ledger-args $import-dir/* >$output-path
    diff -w $expected-path $output-path
  }
}
