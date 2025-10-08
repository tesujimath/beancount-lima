(provide reduce-postings cumulate-postings tabulate-postings)

(require (only-in "lima/lib/list.scm" all))

;; select items which pass all filters, not public, since this is the default behaviour
(define (f/and filters)
  (lambda (x) (all (lambda (f) (f x)) filters)))

;; reducer is a function of three args: acc post txn
(define (reduce-postings directives filters reducer init)
  (let* ((apply-filters (lambda (key-filter-alist)
                          (lambda (x) (all (lambda (key-selector) (let ((filt (hash-try-get filters (car key-selector))))
                                                                    (if filt (filt ((cdr key-selector) x)) #t))) key-filter-alist))))
         (directive-filter (apply-filters `((date . ,directive-date))))
         (posting-filter (apply-filters `((amount . ,posting-amount)
                                          (account . ,posting-account))))
         )
    (transduce directives
               (filtering transaction?)
               (filtering (f/and filters))
               ;; combine each posting with its transaction:
               (mapping (lambda (txn) (transduce (transaction-postings txn)
                                                 (mapping (lambda (p) (cons p txn)))
                                                 (into-list))))
               (flattening)
               (filtering (lambda (p-txn) ((f/and filters) (car p-txn))))
               (into-reducer (lambda (acc p-txn) (reducer acc (car p-txn) (cdr p-txn)))
                             init))))

(define (cumulate-postings directives . filters)
  (reduce-postings directives filters (lambda (acc p txn) (cumulator-reduce acc p)) (cumulator)))

(define (tabulate-postings directives . filters)
  (let* ((reducer (lambda (acc p txn)
                    (cons (list (directive-date txn)
                                (posting-account p)
                                (posting-amount p)
                                (or (transaction-payee txn) "")
                                (or (transaction-narration txn) ""))
                          acc)))
         (postings-with-dates (reverse (reduce-postings directives filters reducer (list)))))
    (tabulate postings-with-dates)))
