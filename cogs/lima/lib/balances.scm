(provide display-balance-sheet)

(require "lima/lib/types.scm")
(require "lima/lib/tabulate.scm")

(define (inventory-for-currencies inv currencies)
  (map (lambda (cur)
         (or (hash-try-get inv cur) ""))
       currencies))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-balance-sheet ldg)
  (let ((all-currencies (ledger-currencies ldg)))
    (cons
     (cons "" all-currencies)
     (transduce (ledger-account-names ldg)
                (compose
                 (mapping (lambda (account-name)
                            (let* ((inv (hash-get (ledger-accounts ldg) account-name)))
                              (cons account-name inv))))
                 (filtering (lambda (pair) (not (hash-empty? (cdr pair)))))
                 (mapping (lambda (pair)
                            (let* ((account-name (car pair))
                                   (inv (cdr pair)))
                              (cons account-name (inventory-for-currencies inv all-currencies))))))
                (into-list)))))

(define (display-balance-sheet ldg)
  (display (tabulate (collate-balance-sheet ldg) 'left 'centre)))
