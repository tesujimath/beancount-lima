(require "lima/prelude.scm")
(require "lima/alist.scm")

(define (inventory-for-currencies inv currencies)
  (map (lambda (cur)
        (alist-get-or-default cur "" inv))
    currencies))

(define (format-balances ledger)
  (let ((all-currencies (ledger-currencies ledger)))
    (cons
      (cons "" all-currencies)
      (transduce (ledger-account-names ledger)
        (compose
          (mapping (lambda (account-name)
                    (let* ((account (hash-get (ledger-accounts ledger) account-name))
                           (inv (account-inventory account)))
                      (cons account-name inv))))
          (filtering (lambda (pair) (not (empty? (cdr pair)))))
          (mapping (lambda (pair)
                    (let* ((account-name (car pair))
                           (inv (cdr pair)))
                      (cons account-name (inventory-for-currencies inv all-currencies))))))
        (into-list)))))

(define (display-balances ledger)
  (display (tabulate (format-balances ledger) 'left 'centre)))

(display-balances *ledger*)
