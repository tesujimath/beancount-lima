(provide
  make-posting-within?)

(require "lima/lib/types.scm")

;; return a predicate which checks whether a posting lies within the period
(define (make-posting-within? p) (lambda (pst) (period-within? p (posting-date pst))))
