(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/types.scm")
(require "lima/posting.scm")

(test-module
  "posting tests"
  (check-equal? "make-posting-within?" ((make-posting-within? (period (date 2025 1 1) (date 2025 2 1))) (posting (date 2025 1 10) (amount (decimal 1050 2) "NZD"))) #t)
  (check-equal? "make-posting-within? not" ((make-posting-within? (period (date 2025 1 1) (date 2025 2 1))) (posting (date 2025 2 10) (amount (decimal 1050 2) "NZD"))) #f))
