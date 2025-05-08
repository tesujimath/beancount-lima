(provide
  make-posting-within?)

(require "lima/types.scm")
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

;; return a predicate which checks whether a posting lies within the period
(define (make-posting-within? p) (lambda (pst) (period-within? p (posting-date pst))))

(test-module
  "posting tests"
  (check-equal? "make-posting-within?" #t ((make-posting-within? (period (date 2025 1 1) (date 2025 2 1))) (posting (date 2025 1 10) (amount (decimal 1050 2) "NZD"))))
  (check-equal? "make-posting-within? not" #f ((make-posting-within? (period (date 2025 1 1) (date 2025 2 1))) (posting (date 2025 2 10) (amount (decimal 1050 2) "NZD")))))
