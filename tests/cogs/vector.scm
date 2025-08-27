(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/vector.scm")

(test-module
  "vector-index tests"
  (check-equal? "vector-index"
    (vector-index (immutable-vector 'a 'b 'c) 'b)
    1)
  (check-equal? "vector-index"
    (vector-index (immutable-vector 'a 'b 'c) 'a)
    0)
  (check-err? "vector-index"
    (vector-index (immutable-vector 'a 'b 'c) 'd)
    'ignored))

(test-module
  "vector-contains? tests"
  (check-equal? "vector-contains?"
    (vector-contains? (immutable-vector 'a 'b 'c) 'b)
    #t)
  (check-equal? "vector-contains?"
    (vector-contains? (immutable-vector 'a 'b 'c) 'a)
    #t)
  (check-equal? "vector-contains?"
    (vector-contains? (immutable-vector 'a 'b 'c) 'd)
    #f))
