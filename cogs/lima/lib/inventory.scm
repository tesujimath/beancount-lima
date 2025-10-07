(provide collect-postings cumulate-postings)

(require "lima/lib/types.scm")
(require "lima/lib/account.scm")
(require "steel/sorting/merge-sort.scm")

(define (collect-postings directives collector
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
               (mapping transaction-postings)
               (flattening)
               (filtering posting-filter)
               (into-for-each collector))))

(define (cumulate-postings directives
                           #:filters
                           [filters (hash)])
  (let ((cum (cumulator)))
    (collect-postings directives (curry cumulator-post cum) #:filters filters)
    cum))
