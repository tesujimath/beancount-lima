(provide
 f/date<
 f/date<=
 f/date>
 f/date>=
 f/date=
 f/date-recent
 f/amount-exceeds
 f/acc
 f/subacc
 f/or)

(require "srfi/srfi-28/format.scm")
(require (only-in "lima/lib/list.scm" all any))

(define (directive-filter f)
  (lambda (x) (if (directive? x) (f x) #t)))

(define (date-filter f d)
  (directive-filter (lambda (x) (f (directive-date x) d))))

;; month and day are optional and default to 1
(define/contract
  (date-with-defaults y md)
  (->/c int? (lambda (x) (and (list? x) (<= (length x) 2) (all int? x))) date?)
  (date y (or (try-list-ref md 0) 1) (or (try-list-ref md 1) 1)))

(define (f/date< y . md) (date-filter date<? (date-with-defaults y md)))
(define (f/date<= y . md) (date-filter date<=? (date-with-defaults y md)))
(define (f/date> y . md) (date-filter date>? (date-with-defaults y md)))
(define (f/date>= y . md) (date-filter date>=? (date-with-defaults y md)))
(define (f/date= y . md) (date-filter date=? (date-with-defaults y md)))

(define/contract
  (f/date-recent days)
  (->/c int? function?)
  (date-filter date>? (date-before (today) days)))


(define (posting-filter f)
  (lambda (x) (if (posting? x) (f x) #t)))

(define (amount-filter f)
  (posting-filter (lambda (x) (f (posting-amount x)))))

(define/contract
  (f/amount-exceeds n)
  (->/c int? function?)
  (amount-filter (lambda (amt) (decimal>=? (decimal-abs (amount-number amt)) (decimal n 0)))))

(define (account-filter f)
  (posting-filter (lambda (x) (f (posting-account x)))))

(define/contract
  (f/acc name)
  (->/c string? function?)
  (account-filter (curry equal? name)))

;; is account or a subaccount of it
(define/contract
  (f/subacc name)
  (->/c string? function?)
  (account-filter (lambda (acc) (or (equal? acc name) (starts-with? acc (format "~a:" name))))))

(define (f/or . filters)
  (lambda (x) (any (lambda (f) (f x)) filters)))
