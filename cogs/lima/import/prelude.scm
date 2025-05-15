(provide
  *imported*)

(require "lima/import/types.scm")
(provide imported
  imported-header
  imported-fields
  imported-transactions
  imported-txnids
  imported-payees
  imported-narrations)

(require "lima/import/display.scm")
(provide format-transaction
  format-balance)

(require "lima/import/ffi.scm")
;; The FFI imported is called `*ffi-imported*`.
;; Here we create a native Steel imported called `*imported*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *imported* (ffi-imported->imported *ffi-imported*))
