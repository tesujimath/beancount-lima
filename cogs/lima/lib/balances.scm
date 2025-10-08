(provide display-balance-sheet)

(define (inventory-units-for-currencies inv currencies)
  (map (lambda (cur)
         (or (hash-try-get (inventory-units inv) cur) ""))
       currencies))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-balance-sheet cum)
  (let ((all-currencies (cumulator-currencies cum)))
    (cons
     (cons "" all-currencies)
     (transduce (cumulator-account-names cum)
                (compose
                 (mapping (lambda (account-name)
                            (let* ((inv (cumulator-account cum account-name)))
                              (cons account-name inv))))
                 (mapping (lambda (pair)
                            (let* ((account-name (car pair))
                                   (inv (cdr pair)))
                              (cons account-name (inventory-units-for-currencies inv all-currencies))))))
                (into-list)))))

(define (display-balance-sheet cum)
  (display (tabulate (collate-balance-sheet cum))))
