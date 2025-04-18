(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(provide __module__)

(define __module__ "tests")

(test-module "auto-posting-tests"
  (let ((current-gbp (FFIRational->rational (hash-get (Account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "GBP"))))
    ;; TODO this should fail!!
    (check-equal? "current-gbp" -6 current-gbp)))
