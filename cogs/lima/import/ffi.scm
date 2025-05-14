(provide
  ffi-imported->imported)

(require "lima/import/types.scm")
(require "lima/alist.scm")

(define (ffi-imported->imported imp)
  (imported (list->alist (ffi-imported-header imp))
    (ffi-imported-fields imp)
    (ffi-imported-transactions imp)
    (ffi-imported-txnids imp)
    (ffi-imported-payees imp)
    (ffi-imported-narrations imp)))
