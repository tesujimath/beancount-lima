(provide
  *imported*)

(require "lima/types.scm")
(require "lima/ffi.scm")

;; The FFI imported is called `*ffi-imported*`.
;; Here we create a native Steel imported called `*imported*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *imported* (ffi-imported->imported *ffi-imported*))
