(require "lima/prelude.scm")
(require "lima/alist.scm")

(define (inventory-for-currencies inv currencies)
  (map (lambda (cur)
        (cdr-assoc-or-default cur "" inv))
    currencies))

(define (format-balances ledger)
  (let ((all-currencies (ledger-currencies ledger)))
    (cons
      (cons "" all-currencies)
      (map
        (lambda (account-name)
          (let* ((account (hash-get (ledger-accounts ledger) account-name))
                 (inv (account-inventory account)) #| ((currencies (ledger-currencies ledger)))|##| (currency-totals (map (lambda (cur))))|#)
            (cons account-name (inventory-for-currencies inv all-currencies))))
        (ledger-account-names ledger)))))

(define (display-balances ledger)
  (display (tabulate (format-balances ledger) 'left 'centre)))

(display-balances *ledger*)
