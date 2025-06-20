(require "lima/prelude.scm")
(require "lima/lib/alist.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let ((current-nzd (alist-get-or-default "NZD" (decimal-zero) (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")))))
  (check-equal? "current-nzd" current-nzd (decimal -2965 2)))
