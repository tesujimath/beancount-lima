(provide *ledger* *options*)

(require "lima/lib/ffi.scm")

;; The FFI ledger is called `*ffi-ledger*`.
;; Here we create a native Steel ledger called `*ledger*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *ledger* (ffi-ledger->ledger *ffi-ledger*))

;; any command line options
(define *options* (ffi-alist->alist *ffi-options*))
