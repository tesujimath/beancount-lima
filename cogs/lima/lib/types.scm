(provide
 inventories
 inventories?
 inventories-currencies
 inventories-main-currency
 inventories-account-names
 inventories-accounts
 period
 period?
 period-within?
 make-period-within?
 make-period-within-previous?
 make-period-since?)

;; a half-open date range, including start, not including end
(struct period (start end) #:transparent)
                                        ; TODO:
                                        ; (define/contract (period start end)
                                        ;   (and (date? start) (date? end)))

(define (period-within? p d) (and (date>=? d (period-start p)) (date<? d (period-end p))))

;; return a predicate which checks whether a date lies within the period
(define (make-period-within? p) (curry period-within? p))

;; return a predicate for the last `n` days
(define (make-period-within-previous? d0 n) (lambda (d) (and (date>? d (date-before d0 n))
                                                             (date<=? d d0))))
;; return a predicate for since d0
(define (make-period-since? d0) (lambda (d)  (date>=? d d0)))

(struct inventories (currencies main-currency account-names accounts) #:transparent)
