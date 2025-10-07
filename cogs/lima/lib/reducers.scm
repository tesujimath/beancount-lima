(provide reduce-postings cumulate-postings tabulate-postings)

;; reducer is a function of three args: acc post txn
(define (reduce-postings directives reducer init
                          #:filters [filters (hash)])
  (let* ((date-filter (hash-try-get filters 'date))
         (account-filter (hash-try-get filters 'account))
         (directive-filter (if date-filter
                               (lambda (d) (date-filter (directive-date d)))
                               (lambda (d) #t)))
         (posting-filter (if account-filter
                             (lambda (p) (account-filter (posting-account p)))
                             (lambda (p) #t))))
    (transduce directives
               (filtering transaction?)
               (filtering directive-filter)
               ;; combine each posting with its transaction:
               (mapping (lambda (txn) (transduce (transaction-postings txn)
                                               (mapping (lambda (p) (cons p txn)))
                                               (into-list))))
               (flattening)
               (filtering (lambda (px) (posting-filter (car px))))
               (into-reducer (lambda (acc px) (reducer acc (car px) (cdr px)))
                             init))))

(define (cumulate-postings directives
                           #:filters
                           [filters (hash)])
  (reduce-postings directives (lambda (acc p txn) (cumulator-reduce acc p)) (cumulator) #:filters filters))

(define (tabulate-postings directives
                           #:filters
                           [filters (hash)])
  (let ((postings-with-dates (reverse (reduce-postings directives (lambda (acc p txn) (cons (list (directive-date txn) (posting-account p) (posting-amount p)) acc)) (list) #:filters filters))))
    (tabulate postings-with-dates)))
