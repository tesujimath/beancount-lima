(provide error)

;; create a new error for anything having an element property,
;; generally directives and postings
(define (error elemental message)
  (ffi-error (alist-get 'element elemental) message))
