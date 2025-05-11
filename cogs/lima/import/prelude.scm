(provide
  *imported*)

(require "lima/import/types.scm")
(provide
  transaction
  transaction-date
  transaction-payee
  transaction-narration
  transaction-amount
  transaction-base-account
  transaction-other-accounts)

(require "lima/import/display.scm")
(provide
  display-transaction)

(require "lima/import/ffi.scm")
;; The FFI imported is called `*ffi-imported*`.
;; Here we create a native Steel imported called `*imported*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *imported* (ffi-imported->imported *ffi-imported*))
