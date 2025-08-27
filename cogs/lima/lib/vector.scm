(provide vector-index vector-contains?)

;; return the smallest index of `x` in `vec`, or fail if not found
(define (vector-index vec x)
  (letrec ((vector-index-acc
             (lambda (vec x n)
               (cond [(>= n (vector-length vec)) (error! x "not found in" vec)]
                 [(equal? (vector-ref vec n) x) n]
                 [else (vector-index-acc vec x (+ n 1))]))))
    (vector-index-acc vec x 0)))
 
;; return whether the vector contains `x` by linear search
(define (vector-contains? vec x)
  (letrec ((vector-contains-acc?
             (lambda (vec x n)
               (cond [(>= n (vector-length vec)) #f]
                 [(equal? (vector-ref vec n) x) #t]
                 [else (vector-contains-acc? vec x (+ n 1))]))))
    (vector-contains-acc? vec x 0)))
