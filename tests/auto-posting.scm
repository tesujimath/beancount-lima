(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(let ((current-gbp (decimal->rational (hash-get (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "GBP"))))
  (check-equal? "current-gbp" -5 current-gbp))

;; fail if any test failures
(when (not (empty? (hash-get (get-test-stats) 'failures)))
  (error! "There were test failures!"))
