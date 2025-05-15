(provide make-infer-secondary-accounts-from-payees-and-narrations)

(require "steel/sorting/merge-sort.scm")
(require "lima/types.scm")
(require "lima/alist.scm")

;; infer expenses account from payees and narrations we found in the ledger
;; the secondary accounts are a list of either strings, or lists of string count category
(define (make-infer-secondary-accounts-from-payees-and-narrations payees narrations)
  (lambda (txn)
    (let* ((amount (amount-number (cdr-assoc 'amount txn)))
           (secondary-accounts
             (cond
               [(decimal>? amount (decimal-zero)) '("Income:Unknown")]
               [(decimal<? amount (decimal-zero))
                 (let* ((found-payee (hash-try-get payees (cdr-assoc-or-default 'payee '() txn)))
                        (found-narration (hash-try-get narrations (cdr-assoc-or-default 'narration '() txn)))
                        (order-accounts (lambda (account-lookup category)
                                         (let* ((account-names (hash-keys->list account-lookup))
                                                (annotated-accounts (map (lambda (account-name)
                                                                          (list account-name (hash-get account-lookup account-name) category))
                                                                     account-names)))
                                           (merge-sort annotated-accounts #:comparator (lambda (row0 row1) (> (cadr row0) (cadr row1))))))))
                   (cond
                     [found-payee (order-accounts found-payee "payee")]
                     [found-narration (order-accounts found-narration "narration")]
                     [else '("Expenses:Unknown")]))]
               [else '()])))
      (cons (cons 'secondary-accounts secondary-accounts) txn))))
