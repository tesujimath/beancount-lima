(provide compose identity)

(define compose (lambda (f g) (lambda (arg) (f (g arg)))))

(define identity (lambda (x) x))
