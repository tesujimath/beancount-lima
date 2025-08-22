(provide
  ffi-alist->alist
  ffi-ledger->ledger)

(require "lima/lib/ledger.scm")
(require "lima/lib/account.scm")
(require "lima/lib/types.scm")
(require "lima/lib/list.scm")

;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.

;; recursively convert ffi-alist to alist,
;; i.e. if any values are lists or ffi-alists they also get converted
(define (ffi-alist->alist xs)
  (if (list? xs)
    (map (lambda (x)
          (cond
            [(ffi-alistitem? x)
                 (cons (string->symbol (ffi-alistitem-key x))
                   (ffi-alist->alist (ffi-alistitem-value x)))]
            [(list? x) (ffi-alist->alist x)]
            [else x]))
      xs)
    xs))

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
    (accounts->ledger accounts (ffi-alist->alist (ffi-ledger-directives ldg)) (ffi-ledger-main-currency ldg) (ffi-alist->alist (ffi-ledger-options ldg)))))
