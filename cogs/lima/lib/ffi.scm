(provide
 ffi-alist->alist
 ffi-ledger->ledger)

(require "lima/lib/ledger.scm")
(require "lima/lib/account.scm")
(require "lima/lib/types.scm")

;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.

(define (ffi-alist->alist alst)
  (map (lambda(item) (cons (string->symbol (ffi-alistitem-key item))
                           (ffi-alistitem-value item)))
       alst))

(define (ffi-amount->amount amt)
  (amount (ffi-amount-number amt) (ffi-amount-currency amt)))

(define (ffi-posting->posting pst)
  (let* ((dt (ffi-posting-date pst))
         (amt (ffi-amount->amount (ffi-posting-amount pst)))
         (flg (ffi-posting-flag pst))
         (opt (if flg (optional-flag flg) '())))
    (posting dt amt opt)))

(define (ffi-account->account acc)
  (postings->account (map ffi-posting->posting (ffi-account-postings acc))))

(define (ffi-ledger->ledger ldg)
  (let* ((ffi-accounts (ffi-ledger-accounts ldg))
         (accounts (transduce (hash-keys->list ffi-accounts)
                              (mapping (lambda (name) (cons name (ffi-account->account (hash-get ffi-accounts name)))))
                              (into-hashmap))))
    (accounts->ledger accounts (ffi-ledger-main-currency ldg) (ffi-alist->alist (ffi-ledger-options ldg)))))
