(provide make-infer-secondary-accounts-from-payees-and-narrations format-secondary-account)

(require "srfi/srfi-28/format.scm")
(require "steel/sorting/merge-sort.scm")
(require "lima/types.scm")
(require "lima/alist.scm")
(require "lima/display/tabulate.scm")

;; infer expenses account from payees and narrations we found in the ledger
;; the secondary accounts are a list of either strings, or lists of string count category
(define (make-infer-secondary-accounts-from-payees-and-narrations payees narrations)
  (lambda (txn)
    (let* ((amount (amount-number (alist-get 'amount txn)))
           (primary-account (alist-get 'primary-account txn))
           (secondary-accounts
             (let* ((found-payee (hash-try-get payees (alist-get-or-empty 'payee txn)))
                    (found-narration (hash-try-get narrations (alist-get-or-empty 'narration txn)))
                    (order-accounts (lambda (account-lookup category)
                                     (let* ((all-account-names (hash-keys->list account-lookup))
                                            (candidate-account-names (filter (lambda (account-name) (not (equal? account-name primary-account))) all-account-names))
                                            (annotated-accounts (map (lambda (account-name)
                                                                      `((name . ,account-name)
                                                                        (infer-count . ,(hash-get account-lookup account-name))
                                                                        (infer-category . ,category)))
                                                                 candidate-account-names)))
                                       (merge-sort annotated-accounts
                                         #:comparator
                                         ;; by infer-count descending, then by name ascending
                                         (lambda (acc0 acc1) (cond [(> (alist-get 'infer-count acc0)
                                                                      (alist-get 'infer-count acc1))
                                                                    #t]
                                                              [(< (alist-get 'infer-count acc0)
                                                                  (alist-get 'infer-count acc1))
                                                                #f]
                                                              [else (string<? (alist-get 'name acc0)
                                                                     (alist-get 'name acc1))])))))))
               (cond
                 [found-payee (order-accounts found-payee "payee")]
                 [found-narration (order-accounts found-narration "narration")]
                 [(decimal>? amount (decimal-zero)) (list (list (cons 'name "Income:Unknown")))]
                 [(decimal<? amount (decimal-zero)) (list (list (cons 'name "Expenses:Unknown")))]
                 [else '()]))))
      (cons (cons 'secondary-accounts secondary-accounts) txn))))

;; format the account name inferred above with a comment about where it came from
(define (format-secondary-account acc width)
  (let* ((name (alist-get 'name acc))
         (pad (spaces (max (- width (string-length name)) 0)))
         (count (alist-get-or-empty 'infer-count acc))
         (category (alist-get-or-empty 'infer-category acc)))
    (if (or (empty? count) (empty? category))
      name
      (let ((suffix (if (> count 1) "s" "")))
        (format "~a~a ; inferred from ~a ~a~a" name pad count category suffix)))))
