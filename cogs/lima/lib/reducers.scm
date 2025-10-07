(provide reduce-postings cumulate-postings tabulate-postings)

(require (only-in "lima/lib/list.scm" all))

;; reducer is a function of three args: acc post txn
(define (reduce-postings directives reducer init
                          #:filters [filters (hash)])
  (let* ((apply-filters (lambda (key-filter-alist)
                          (lambda (x) (all (lambda (key-selector) (let ((filt (hash-try-get filters (car key-selector))))
                                                          (if filt (filt ((cdr key-selector) x)) #t))) key-filter-alist))))
         (directive-filter (apply-filters `((date . ,directive-date))))
         (posting-filter (apply-filters `((amount . ,posting-amount)
                                          (account . ,posting-account))))
         )
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
