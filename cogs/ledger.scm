;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.
(struct ledger (accounts))

;; the FFI ledger is called *ffi-ledger*
; here we create a native Steel ledger called *ledger*
(define *ledger*
  (ledger (Ledger-accounts *ffi-ledger*)))
