(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(set-test-mode!)

(provide __module__)

(define __module__ "tests")

(test-module "auto-posting-tests"
  (let ((current-gbp (Decimal->rational (hash-get (Account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "GBP"))))
    (check-equal? "current-gbp" -5 current-gbp)))

;; -------------- Report ------------------

(require "steel/lists/lists.scm")
(define auto-posting-test-stats (get-test-stats))

(displayln "Passed: " (hash-ref auto-posting-test-stats 'success-count))
(displayln "Skipped compilation (expected failure): " (hash-ref auto-posting-test-stats 'failed-to-compile))
(displayln "Failed: " (hash-ref auto-posting-test-stats 'failure-count))

(for-each (lambda (x) (displayln "    > " x)) (hash-ref auto-posting-test-stats 'failures))

(when (not (empty? (hash-get (get-test-stats) 'failures)))
  (error! "There were test failures!"))
