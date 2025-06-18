(provide string-empty?)

(define (string-empty? s) (eq? (string-length s) 0))
