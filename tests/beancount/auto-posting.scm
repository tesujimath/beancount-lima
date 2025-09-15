(require "lima/lib/prelude.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let ((current-nzd (or (hash-try-get
                        (inventory-units (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current"))
                        "NZD")
                       (decimal-zero))))
  (check-equal? "current-nzd" current-nzd (decimal -2965 2)))
