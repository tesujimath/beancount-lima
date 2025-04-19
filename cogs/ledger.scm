;; convert FFIRational to native rational
(define (FFIRational->rational r) (/ (FFIRational-numerator r) (FFIRational-denominator r)))

;; Steel does not allow construction of arbitrary native Steel values from Rust,
;; so we convert from FFI values to native values here.
(struct ledger (currencies account-names accounts))

;; The FFI ledger is called `*ffi-ledger*`.
;; Here we create a native Steel ledger called `*ledger*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *ledger*
  (ledger
    (Ledger-currencies *ffi-ledger*)
    (Ledger-account-names *ffi-ledger*)
    (Ledger-accounts *ffi-ledger*)))
