(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/list.scm")

(test-module
  "list-index tests"
  (check-equal? "list-index"
    (list-index '(a b c) 'b)
    1))

(test-module
  "all tests"
  (check-equal? "all true"
    (all even? '(2 6 4 100))
    #t)
  (check-equal? "all false"
    (all even? '(2 6 3 100))
    #f)
  (check-equal? "all empty"
    (all even? '())
    #t))

(test-module
  "any tests"
  (check-equal? "any true"
    (any even? '(1 2 3 10))
    #t)
  (check-equal? "any false"
    (any even? '(3 9 15))
    #f)
  (check-equal? "any empty"
    (any even? '())
    #f))
