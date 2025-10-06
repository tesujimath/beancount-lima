(provide string-empty? string-remove-prefix)

(define (string-empty? s) (eq? (string-length s) 0))

;; if s starts with prefix return a copy with prefix removed, else original string
(define (string-remove-prefix s p)
  (if (starts-with? s p)
      (substring s (string-length p) (string-length s))
      s))
