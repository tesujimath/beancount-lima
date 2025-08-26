(provide
  ffi-alist->alist
  ffi-ledger->ledger)

(require "lima/lib/ledger.scm")

;; TODO eliminate this
;; recursively convert ffi-alist to alist,
;; i.e. if any values are lists or ffi-alists they also get converted
(define (ffi-alist->alist xs)
  (if (list? xs)
    (map (lambda (x)
          (cond
            [(ffi-alistitem? x)
                 (cons (string->symbol (ffi-alistitem-key x))
                   (ffi-alist->alist (ffi-alistitem-value x)))]
            [(list? x) (ffi-alist->alist x)]
            [else x]))
      xs)
    xs))

(define (ffi-ledger->ledger ldg)
  (directives->ledger (ffi-ledger-directives ldg) (ffi-ledger-options ldg)))
