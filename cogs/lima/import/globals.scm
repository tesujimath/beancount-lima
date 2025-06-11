(provide *import-group*)

(require "lima/import/ffi.scm")
;; The FFI import group is called `*ffi-import-group*`.
;; Here we create a native Steel import-group called `*import-group*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *import-group* (ffi-import-group->import-group *ffi-import-group*))
