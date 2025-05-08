(provide tax-year)

(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

;; Period for local tax year.
;; This example is for April 1 of the previous year to March 31 of year `y`,
;; noting that period is a half-open date range.
(define (tax-year y) (period (date (- y 1) 4 1) (date y 4 1)))

(test-module
  "tax-year tests"
  (check-equal? "start of tax year" #t (period-within? (tax-year 2025) (date 2024 4 1)))
  (check-equal? "before tax year" #f (period-within? (tax-year 2025) (date 2024 3 31)))
  (check-equal? "end of tax year" #t (period-within? (tax-year 2025) (date 2025 3 31)))
  (check-equal? "after tax year" #f (period-within? (tax-year 2025) (date 2025 4 1))))
