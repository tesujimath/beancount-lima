(provide
  decimal->rational
  ledger
  ledger?
  ledger-currencies
  ledger-account-names
  ledger-accounts
  account
  account?
  account-inventory
  account-postings
  period
  period?
  period-within?
  make-period-within?
  posting
  posting?
  posting-date
  posting-amount
  posting-flag
  make-posting-flagged-with?
  amount
  amount?
  amount-number
  amount-currency)

;; convert decimal to native rational
(define (decimal->rational r) (/ (decimal-numerator r) (decimal-denominator r)))

;; a half-open date range, including start, not including end
(struct period (start end) #:transparent)
; TODO:
; (define/contract (period start end)
;   (and (date? start) (date? end)))

(define (period-within? p d) (and (date>=? d (period-start p)) (date<? d (period-end p))))

;; return a predicate which checks whether a date lies within the period
(define (make-period-within? p) (curry period-within? p))

(struct amount (number currency) #:transparent)

(struct posting (date amount flag) #:transparent)

;; return a predicate which selects postings with given flag
(define (make-posting-flagged-with? flag)
  (lambda (pst) (equal? (posting-flag pst) flag)))

(struct account (inventory postings) #:transparent)

(struct ledger (currencies account-names accounts) #:transparent)
