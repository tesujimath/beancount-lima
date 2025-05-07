(require (for-syntax "steel/tests/unit-test.scm"))

(let ((current-nzd (hash-get (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "NZD")))
  (check-equal? "current-nzd" current-nzd (decimal -965 2)))
