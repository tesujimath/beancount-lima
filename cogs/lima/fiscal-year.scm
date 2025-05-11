(provide fy make-fy?)

(require "lima/count/prelude.scm")
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

;; Period for fiscal year.
;; This example is for April 1 of the previous year to March 31 of year `y`,
;; noting that period is a half-open date range.
(define (fy y) (period (date (- y 1) 4 1) (date y 4 1)))

;; Predicate for date being within a given fiscal year.
(define (make-fy? y) (make-period-within? (fy y)))

(test-module
  "fy tests"
  (check-equal? "start of tax year" (period-within? (fy 2025) (date 2024 4 1)) #t)
  (check-equal? "before tax year" (period-within? (fy 2025) (date 2024 3 31)) #f)
  (check-equal? "end of tax year" (period-within? (fy 2025) (date 2025 3 31)) #t)
  (check-equal? "after tax year" (period-within? (fy 2025) (date 2025 4 1)) #f))
