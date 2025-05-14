(provide flatmap list-index all any)

(define (flatmap proc seq)
  (letrec ((accumulate
             (lambda (op initial sequence)
               (if (null? sequence)
                 initial
                 (op (car sequence)
                   (accumulate op initial (cdr sequence)))))))
    (accumulate append '() (map proc seq))))

;; example
;; (flatmap (lambda (x) (list 'freddy x)) '(1 2 3))
;; => '(freddy 1 freddy 2 freddy 3)

;; return the smallest index of `x` in `lst`, or fail if not found
(define (list-index lst x)
  (letrec ((list-index-acc
             (lambda (lst x n)
               (cond [(empty? lst) (error! "not found")]
                 [(equal? (car lst) x) n]
                 [else (list-index-acc (cdr lst) x (+ n 1))]))))
    (list-index-acc lst x 0)))

(define (all predicate lst)
  (foldl (lambda (item ok) (and ok (predicate item))) #t lst))

(define (any predicate lst)
  (foldl (lambda (item ok) (or ok (predicate item))) #f lst))
