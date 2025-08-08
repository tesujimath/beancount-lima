#!/usr/bin/env elvish

use os
use str

# all import tests currently run with the same config
set-env LIMA_COGPATH (pwd)/tests/import.config:$E:LIMA_COGPATH

with pwd = tests/import {
  for expected-path [*[nomatch-ok].expected.beancount] {
    var import-dir = (str:replace .expected.beancount "" $expected-path)
    var ledger-path = (str:replace .expected "" $expected-path)
    var output-path = (str:replace .expected .actual $expected-path)
    var ledger-args = [--ledger $ledger-path]
    echo
    echo lima -o standalone $@ledger-args import $import-dir/*
    lima -o standalone $@ledger-args import $import-dir/* >$output-path
    diff -w $expected-path $output-path
  }
}
