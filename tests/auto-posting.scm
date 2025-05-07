(let ((current-gbp (decimal->rational (hash-get (account-inventory (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")) "GBP"))))
  (check-equal? "current-gbp" -5 current-gbp))
