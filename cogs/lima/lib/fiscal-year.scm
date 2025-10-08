(provide f/date-fy)

(require (only-in "lima/lib/filters.scm"
                  f/and
                  f/date>=
                  f/date<))

;; Period for fiscal year.
;; This example is for April 1 of the previous year to March 31 of year `y`,
(define/contract
  (f/date-fy y)
  (->/c int? function?)
  (f/and (f/date> (- y 1) 4) (f/date< y 4)))
