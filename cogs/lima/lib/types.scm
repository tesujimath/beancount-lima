(provide
  decimal->rational
  ledger
  ledger?
  ledger-currencies
  ledger-main-currency
  ledger-account-names
  ledger-accounts
  ledger-options
  account
  account?
  account-inventory
  account-postings
  period
  period?
  period-within?
  make-period-within?
  optional-flag
  posting
  posting?
  posting-date
  posting-amount
  posting-has-flag
  posting-flag
  make-posting-flagged-with?
  amount
  amount?
  amount-number
  amount-currency
  transaction?
  price?
  balance?
  open?
  close?
  commodity?
  pad?
  document?
  note?
  event?
  query? #| TODO reinstate posting?|#)

(require "lima/lib/alist.scm")

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

;; create optional with flag
(define (optional-flag flg) `((flag . ,flg)))

(struct posting (date amount optional) #:transparent)
(define (posting-has-flag pst)
  (alist-contains? 'flag (posting-optional pst)))
(define (posting-flag pst)
  (alist-get 'flag (posting-optional pst)))

;; return a predicate which selects postings with given flag
(define (make-posting-flagged-with? flag)
  (lambda (pst) (and (posting-has-flag pst)
                 (equal? (posting-flag pst) flag))))

(struct account (inventory postings) #:transparent)

(struct ledger (currencies main-currency account-names accounts options) #:transparent)

;; tests for elements
(define (specified-element? element-f x)
  (and (alist? x)
    (let ((element (alist-try-get 'element x)))
      (and element (element-f element)))))
(define (transaction? x) (specified-element? ffi-element-transaction? x))
(define (price? x) (specified-element? ffi-element-price? x))
(define (balance? x) (specified-element? ffi-element-balance? x))
(define (open? x) (specified-element? ffi-element-open? x))
(define (close? x) (specified-element? ffi-element-close? x))
(define (commodity? x) (specified-element? ffi-element-commodity? x))
(define (pad? x) (specified-element? ffi-element-pad? x))
(define (document? x) (specified-element? ffi-element-document? x))
(define (note? x) (specified-element? ffi-element-note? x))
(define (event? x) (specified-element? ffi-element-event? x))
(define (query? x) (specified-element? ffi-element-query? x))

; TODO reinstate when posting struct removed (define (posting? x) (specified-element? ffi-element-posting? x))
