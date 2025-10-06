(provide directives->inventories)

(require "lima/lib/types.scm")
(require "lima/lib/account.scm")
(require "steel/sorting/merge-sort.scm")

(define (directives->inventories directives
                                 #:date-filter
                                 [date-filter #f]
                                 #:account-filter
                                 [account-filter #f])
  (let ((invs-builder (inventories-builder))
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
               (into-for-each (lambda (post)
                                (inventories-builder-post invs-builder post))))
    (let*
        ((currencies (inventories-builder-currencies invs-builder))
         (main-currency (inventories-builder-main-currency invs-builder))
         (inv (inventories-builder-build invs-builder))
         (account-names (merge-sort (hash-keys->list inv) #:comparator string<?)))
      (inventories currencies
                   main-currency
                   account-names
                   inv))))
