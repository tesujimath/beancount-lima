(provide make-infer-secondary-accounts-from-payees-and-narrations)

(require "srfi/srfi-28/format.scm")
(require "steel/sorting/merge-sort.scm")
(require "lima/lib/types.scm")
(require "lima/lib/tabulate.scm")

;; infer expenses account from payees and narrations we found in the ledger
;; the secondary accounts are a list of either strings, or lists of string count category
(define (make-infer-secondary-accounts-from-payees-and-narrations payees narrations)
  (lambda (txn)
    (let* ((amount (amount-number (hash-get txn 'amount)))
           (primary-account (hash-get txn 'primary-account))
           (secondary-accounts
             (let* ((found-payee (hash-try-get payees (or (hash-try-get txn 'payee) '())))
                    (found-narration (hash-try-get narrations (or (hash-try-get txn 'narration) '())))
                    (order-accounts (lambda (account-lookup category)
                                     (let* ((all-account-names (hash-keys->list account-lookup))
                                            (candidate-account-names (filter (lambda (account-name) (not (equal? account-name primary-account))) all-account-names))
                                            (annotated-accounts (map (lambda (account-name)
                                                                      (hash 'name account-name
                                                                            'infer-count (hash-get account-lookup account-name)
                                                                            'infer-category category))
                                                                 candidate-account-names)))
                                       (merge-sort annotated-accounts
                                         #:comparator
                                         ;; by infer-count descending, then by name ascending
                                         (lambda (acc0 acc1) (cond [(> (hash-get acc0 'infer-count)
                                                                      (hash-get acc1 'infer-count))
                                                                    #t]
                                                              [(< (hash-get acc0 'infer-count)
                                                                  (hash-get acc1 'infer-count))
                                                                #f]
                                                              [else (string<? (hash-get acc0 'name)
                                                                     (hash-get acc1 'name))])))))))
               (cond
                 [found-payee (order-accounts found-payee "payee")]
                 [found-narration (order-accounts found-narration "narration")]
                 [(decimal>? amount (decimal-zero)) (list (hash 'name "Income:Unknown"))]
                 [(decimal<? amount (decimal-zero)) (list (hash 'name "Expenses:Unknown"))]
                 [else '()]))))
      (hash-insert txn 'secondary-accounts secondary-accounts))))
