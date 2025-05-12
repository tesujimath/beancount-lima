(provide
  ffi-imported->imported)

(require "lima/import/types.scm")

(define (ffi-imported->imported imp)
  (imported (ffi-imported-transaction-fields imp) (ffi-imported-transactions imp)))
