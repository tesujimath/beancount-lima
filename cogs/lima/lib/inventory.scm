(provide directives->inventories)

(require "lima/lib/types.scm")
(require "lima/lib/account.scm")
(require "steel/sorting/merge-sort.scm")

(define (directives->inventories directives)
  (let ((invs-builder (inventories-builder)))
    (transduce directives
               (filtering transaction?)
               (mapping transaction-postings)
               (flattening)
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

;; Return a new ledger filtered by `predicates`, which is an alist with the following keys:
;; `account-name` a predicate that takes an account name
;; `date` a predicate that takes a posting date
;;
;; The result is the intersection of all predicates.
;;
;; (define (inventories-filter predicates ldg)
;;   (let* ((filtered-account-name? (combined-predicate 'account-name predicates))
;;                                         ; (filtered-date? (combined-predicate 'date predicates))
;;                                         ; (filtered-posting? (lambda (pst) (filtered-date? (posting-date pst))))

;;          (filtered-account-names
;;           (filter filtered-account-name?
;;                   (inventories-account-names ldg)))

;;          (filtered-accounts
;;           (transduce filtered-account-names
;;                      (mapping (lambda (name)
;;                                 (let* ((acc (hash-get (inventories-accounts ldg) name))
;;                                         ; (filtered-acc (postings->account (filter filtered-posting? (account-postings acc))))
;;                                         ; (not-filtered-acc (postings->account (account-postings acc))))
;;                                         ; TODO fix this
;;                                        (empty-todo '()))
;;                                   (cons name empty-todo))))
;;                      (into-hashmap))))
;;     (ledger
;;      (inventories-currencies ldg)
;;      (inventories-main-currency ldg)
;;      filtered-account-names
;;      filtered-accounts)))
