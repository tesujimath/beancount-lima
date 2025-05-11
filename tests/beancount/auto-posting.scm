(require "lima/count/prelude.scm")
(require "lima/alist.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let ((current-nzd (cdr-assoc-or-default "NZD" (decimal-zero) (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")))))
  (check-equal? "current-nzd" current-nzd (decimal -2965 2)))
