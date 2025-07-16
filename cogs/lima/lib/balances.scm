(provide display-balances)

(require "lima/lib/types.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/tabulate.scm")

(define (inventory-for-currencies inv currencies)
  (map (lambda (cur)
        (alist-get-or-default cur "" inv))
    currencies))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-balances ldg)
  (let ((all-currencies (ledger-currencies ldg)))
    (cons
      (cons "" all-currencies)
      (transduce (ledger-account-names ldg)
        (compose
          (mapping (lambda (account-name)
                    (let* ((account (hash-get (ledger-accounts ldg) account-name))
                           (inv (account-inventory account)))
                      (cons account-name inv))))
          (filtering (lambda (pair) (not (empty? (cdr pair)))))
          (mapping (lambda (pair)
                    (let* ((account-name (car pair))
                           (inv (cdr pair)))
                      (cons account-name (inventory-for-currencies inv all-currencies))))))
        (into-list)))))

(define (display-balances ldg)
  (display (tabulate (collate-balances ldg) 'left 'centre)))
