(provide
  decimal->rational
  ledger
  ledger?
  ledger-currencies
  ledger-account-names
  ledger-accounts
  ffi-ledger->ledger
  account
  account?
  account-inventory
  account-postings
  posting
  posting?
  posting-date
  posting-amount
  amount
  amount?
  amount-number
  amount-currency)

;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.

;; convert decimal to native rational
(define (decimal->rational r) (/ (decimal-numerator r) (decimal-denominator r)))

(struct amount (number currency))
(define (ffi-amount->amount amt)
  (amount (ffi-amount-number amt) (ffi-amount-currency amt)))

(struct posting (date amount))
(define (ffi-posting->posting pst)
  (posting (ffi-posting-date pst) (ffi-amount->amount (ffi-posting-amount pst))))

(struct account (inventory postings))
(define (ffi-account->account acc)
  (account (ffi-account-inventory acc) (map ffi-posting->posting (ffi-account-postings acc))))

(struct ledger (currencies account-names accounts))

(define (ffi-ledger->ledger ldg)
  (let* ((account-names (ffi-ledger-account-names ldg))
         (ffi-accounts (ffi-ledger-accounts ldg))
         (accounts (transduce account-names
                    (mapping (lambda (name) (cons name (ffi-account->account (hash-get ffi-accounts name)))))
                    (into-hashmap))))
    (ledger
      (ffi-ledger-currencies ldg)
      account-names
      accounts)))
