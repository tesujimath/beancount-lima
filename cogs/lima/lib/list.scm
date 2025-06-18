(provide flatmap list-index all any find find-and-map-or-default partition)

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

(define (find predicate lst)
  (cond
    [(empty? lst) #f]
    [(predicate (car lst)) (car lst)]
    [else (find predicate (cdr lst))]))

(define (find-and-map-or-default predicate lst f default)
  (let ((found (find predicate lst)))
    (if found
      (f found)
      default)))

;; partition a list by predicate returning a pair
;; whose car is the list of elements for which the predicate is true
(define (partition predicate lst)
  (let ((partitioned
          (foldl (lambda (item acc) (if (predicate item)
                                     (cons (cons item (car acc)) (cdr acc))
                                     (cons (car acc) (cons item (cdr acc)))))
            '(() . ())
            lst)))
    (cons (reverse (car partitioned)) (reverse (cdr partitioned)))))
