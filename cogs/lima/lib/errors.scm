(provide error)

;; create a new error for anything having an element property,
;; generally directives and postings
(define (error elemental message)
  (ffi-error (or (alist-try-get 'element elemental)
                 (error! "error can only be used on objects with an element field, generally directives or postings"))
             message))
