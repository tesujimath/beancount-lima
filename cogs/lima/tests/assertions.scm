;; caller must (require (for-syntax "steel/tests/unit-test.scm"))
(provide (for-syntax check?))

(define-syntax check?
  (syntax-rules ()
    [(check? 'skip rest ...) (check-equal? 'skip . rest)]
    [(check? name input)
      (check-equal? name input #t)]))
