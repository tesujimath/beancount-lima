(provide
  ffi-import-group->import-group)

(require "lima/lib/import/types.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/ffi.scm")

(define (ffi-import-source->import-source imp)
  (import-source (ffi-alist->alist (ffi-import-source-header imp))
    (ffi-import-source-fields imp)
    (ffi-import-source-transactions imp)))

(define (ffi-import-group->import-group imp)
  (import-group (map ffi-import-source->import-source (ffi-import-group-sources imp))
    (list->hashset (ffi-import-group-txnids imp))
    (ffi-import-group-payees imp)
    (ffi-import-group-narrations imp)))
