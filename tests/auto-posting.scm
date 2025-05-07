(require (for-syntax "lima/tests/assertions.scm"))
(require (for-syntax "steel/tests/unit-test.scm"))

(let ((current-nzd (hash-get (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "NZD")))
  (check? "current-nzd" (decimal=? (decimal -965 2) current-nzd)))
