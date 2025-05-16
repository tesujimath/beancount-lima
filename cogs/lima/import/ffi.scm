(provide
  ffi-imported->imported)

(require "lima/import/types.scm")
(require "lima/alist.scm")
(require "lima/ffi.scm")

(define (ffi-imported->imported imp)
  (imported (ffi-alist->alist (ffi-imported-header imp))
    (ffi-imported-fields imp)
    (ffi-imported-transactions imp)
    (list->hashset (ffi-imported-txnids imp))
    (ffi-imported-payees imp)
    (ffi-imported-narrations imp)))
