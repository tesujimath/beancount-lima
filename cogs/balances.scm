(define (inventory-for-currencies-as-strings inv currencies)
  (map (lambda (cur)
        (if (hash-contains? inv cur)
          (decimal->string (hash-get inv cur))
          ""))
    currencies))

(define (balances ledger)
  (let ((all-currencies (ledger-currencies ledger)))
    (cons
      (cons "" all-currencies)
      (map
        (lambda (account-name)
          (let* ((account (hash-get (ledger-accounts ledger) account-name))
                 (inv (Account-inventory account)) #| ((currencies (ledger-currencies ledger)))|##| (currency-totals (map (lambda (cur))))|#)
            (cons account-name (inventory-for-currencies-as-strings inv all-currencies))))
        (ledger-account-names ledger)))))

(define (display-balances ledger)
  (display (tabulate (balances ledger))))

(display-balances *ledger*)
