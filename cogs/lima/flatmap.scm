(provide flatmap)

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; example
;; (flatmap (lambda (x) (list 'freddy x)) '(1 2 3))
;; => '(freddy 1 freddy 2 freddy 3)
