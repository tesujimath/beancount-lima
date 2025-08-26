(provide
  directives->ledger
  ledger-filter
  ;; for testing
  combined-predicate)

(require "lima/lib/types.scm")
(require "lima/lib/account.scm")
(require "steel/sorting/merge-sort.scm")

(define (directives->ledger directives opts)
  (let ((inv-accum (inventory-accumulator)))
    (transduce directives
      (filtering transaction?)
      (mapping transaction-postings)
      (flattening)
      (into-for-each(lambda (post)
                     (inventory-accumulator-post inv-accum post))))
    (let*
      ((currencies (inventory-accumulator-currencies inv-accum))
        (main-currency (inventory-accumulator-main-currency inv-accum))
        (inv (inventory-accumulator-build inv-accum))
        (account-names (merge-sort (hash-keys->list inv) #:comparator string<?)))
      (ledger (merge-sort currencies #:comparator string<?)
        main-currency
        account-names
        inv
        directives
        opts))))

;; return a new ledger filtered by account name `predicate`
(define (ledger-filter-accounts-by-name predicate ldg)
  (let* ((account-names (filter predicate (ledger-account-names ldg)))
         (accounts (transduce account-names
                    (mapping (lambda (name) (cons name (hash-get (ledger-accounts ldg) name))))
                    (into-hashmap))))
    (ledger
      (ledger-currencies ldg)
      account-names
      accounts
      (ledger-directives ldg)
      (ledger-options ldg))))

;; transducer to extract the combined predicate from a predicates alist
;; example:
;; (define preds (list (cons 'date 1) (cons 'account-name (lambda (s) (string-contains? s "ab"))) (cons 'account-name (lambda (s) (string-contains? s "bc")))))
;; (define preds? (transduce preds (combined-predicate 'account-name) (into-list)))
(define (combined-predicate key preds)
  (lambda (x)
    (transduce preds
      (compose
        (filtering (lambda (pair) (eq? key (car pair))))
        (mapping (lambda (pair) ((cdr pair) x))))
      (into-reducer (lambda (a p) (and a p)) #t))))

;; Return a new ledger filtered by `predicates`, which is an alist with the following keys:
;; `account-name` a predicate that takes an account name
;; `date` a predicate that takes a posting date
;;
;; The result is the intersection of all predicates.
;;
;; WIP TODO removing date from posting
(define (ledger-filter predicates ldg)
  (let* ((filtered-account-name? (combined-predicate 'account-name predicates))
         ; (filtered-date? (combined-predicate 'date predicates))
         ; (filtered-posting? (lambda (pst) (filtered-date? (posting-date pst))))

         (filtered-account-names
           (filter filtered-account-name?
             (ledger-account-names ldg)))

         (filtered-accounts
           (transduce filtered-account-names
             (mapping (lambda (name)
                       (let* ((acc (hash-get (ledger-accounts ldg) name))
                              ; (filtered-acc (postings->account (filter filtered-posting? (account-postings acc))))
                              ; (not-filtered-acc (postings->account (account-postings acc))))
                              ; TODO fix this
                              (empty-todo '()))
                         (cons name empty-todo))))
             (into-hashmap))))
    (ledger
      (ledger-currencies ldg)
      (ledger-main-currency ldg)
      filtered-account-names
      filtered-accounts
      (ledger-options ldg))))
