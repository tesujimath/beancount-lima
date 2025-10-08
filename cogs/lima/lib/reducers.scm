(provide display-directives reduce-postings cumulate-postings tabulate-postings)

(require (only-in "lima/lib/list.scm" all))
(require (only-in "lima/lib/filters.scm" f/and))

;; reducer is a function of two args: acc d
(define (display-directives directives . filters)
  (transduce directives
             (filtering (apply f/and filters))
             (into-for-each displayln)))

;; reducer is a function of three args: acc post txn
(define (reduce-postings directives filters reducer init)
  (transduce directives
             (filtering transaction?)
             (filtering (apply f/and filters))
             ;; combine each posting with its transaction:
             (mapping (lambda (txn) (transduce (transaction-postings txn)
                                               (mapping (lambda (p) (cons p txn)))
                                               (into-list))))
             (flattening)
             (filtering (lambda (p-txn) ((apply f/and filters) (car p-txn))))
             (into-reducer (lambda (acc p-txn) (reducer acc (car p-txn) (cdr p-txn)))
                           init)))

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
