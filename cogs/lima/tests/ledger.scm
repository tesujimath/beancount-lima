(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))
(require "lima/types.scm")
(require "lima/account.scm")
(require "lima/ledger.scm")

(test-module
  "ledger tests"
  (let ((p1 (list
             (cons 'account-name (make-subaccount? "Assets"))
             (cons 'date (make-period-within? (period (date 2025 1 1) (date 2025 2 1))))
             (cons 'account-name (make-subaccount? "Assets:Bank")))))
    (check-equal? "combined-predicate subaccount 1" ((combined-predicate 'account-name p1) "Assets:Bank")
      #t)
    (check-equal? "combined-predicate subaccount 2 not" ((combined-predicate 'account-name p1) "Assets")
      #f)
    (check-equal? "combined-predicate date 1" ((combined-predicate 'date p1) (date 2025 1 3))
      #t)
    (check-equal? "combined-predicate date 2 not" ((combined-predicate 'date p1) (date 2025 2 2))
      #f)))
