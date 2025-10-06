(provide display-balance-sheet)

(require "lima/lib/types.scm")

(define (inventory-units-for-currencies inv currencies)
  (map (lambda (cur)
         (or (hash-try-get (inventory-units inv) cur) ""))
       currencies))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-balance-sheet ldg)
  (let ((all-currencies (inventories-currencies ldg)))
    (cons
     (cons "" all-currencies)
     (transduce (inventories-account-names ldg)
                (compose
                 (mapping (lambda (account-name)
                            (let* ((inv (hash-get (inventories-accounts ldg) account-name)))
                              (cons account-name inv))))
                 (mapping (lambda (pair)
                            (let* ((account-name (car pair))
                                   (inv (cdr pair)))
                              (cons account-name (inventory-units-for-currencies inv all-currencies))))))
                (into-list)))))

(define (display-balance-sheet ldg)
  (display (tabulate (collate-balance-sheet ldg))))
