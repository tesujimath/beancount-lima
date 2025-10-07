(provide
 f/date<
 f/date<=
 f/date>
 f/date>=
 f/date=
 f/date-recent
 f/amount-exceeds
 f/acc
 f/subacc)

(require "srfi/srfi-28/format.scm")

(define (directive-filter f)
  (lambda (x) (if (directive? x) (f x) #t)))

(define (date-filter f d)
  (directive-filter (lambda (x) (f (directive-date x) d))))

(define (f/date< d) (date-filter date<? d))
(define (f/date<= d) (date-filter date<=? d))
(define (f/date> d) (date-filter date>? d))
(define (f/date>= d) (date-filter date>=? d))
(define (f/date= d) (date-filter date=? d))
(define (f/date-recent n) (date-filter date>? (date-before (today) n)))


(define (posting-filter f)
  (lambda (x) (if (posting? x) (f x) #t)))

(define (amount-filter f)
  (posting-filter (lambda (x) (f (posting-amount x)))))

(define (f/amount-exceeds n) (amount-filter (lambda (amt) (decimal>=? (decimal-abs (amount-number amt)) (decimal n 0)))))

(define (account-filter f)
  (posting-filter (lambda (x) (f (posting-account x)))))

(define (f/acc name) (account-filter (curry equal? name)))
;; is account or a subaccount of it
(define (f/subacc name) (account-filter (lambda (acc) (or (equal? acc name) (starts-with? acc (format "~a:" name))))))
