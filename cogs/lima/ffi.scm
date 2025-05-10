(provide
  ffi-ledger->ledger
  ffi-imported->imported)

(require "lima/ledger.scm")
(require "lima/account.scm")
(require "lima/types.scm")

;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.

(define (ffi-amount->amount amt)
  (amount (ffi-amount-number amt) (ffi-amount-currency amt)))

(define (ffi-posting->posting pst)
  (posting (ffi-posting-date pst) (ffi-amount->amount (ffi-posting-amount pst))))

(define (ffi-account->account acc)
  (postings->account (map ffi-posting->posting (ffi-account-postings acc))))

(define (ffi-ledger->ledger ldg)
  (let* ((ffi-accounts (ffi-ledger-accounts ldg))
         (accounts (transduce (hash-keys->list ffi-accounts)
                    (mapping (lambda (name) (cons name (ffi-account->account (hash-get ffi-accounts name)))))
                    (into-hashmap))))
    (accounts->ledger accounts)))

(define (ffi-imported->imported imp)
  (imported (ffi-imported-transaction-fields imp) (ffi-imported-transactions imp)))
