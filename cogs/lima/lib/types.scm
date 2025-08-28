(provide
  ledger
  ledger?
  ledger-currencies
  ledger-main-currency
  ledger-account-names
  ledger-accounts
  period
  period?
  period-within?
  make-period-within?
  optional-flag)

;; a half-open date range, including start, not including end
(struct period (start end) #:transparent)
; TODO:
; (define/contract (period start end)
;   (and (date? start) (date? end)))

(define (period-within? p d) (and (date>=? d (period-start p)) (date<? d (period-end p))))

;; return a predicate which checks whether a date lies within the period
(define (make-period-within? p) (curry period-within? p))

;; create optional with flag
(define (optional-flag flg) `((flag . ,flg)))

(struct ledger (currencies main-currency account-names accounts) #:transparent)
