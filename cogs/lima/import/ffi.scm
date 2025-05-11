(provide
  ffi-imported->imported)

(require "lima/import/types.scm")

(define (ffi-imported->imported imp)
  (imported (ffi-imported-raw-transaction-fields imp) (ffi-imported-raw-transactions imp)))
