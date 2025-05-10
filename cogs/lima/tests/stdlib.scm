(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/stdlib.scm")

(test-module
  "stdlib tests"
  (check-equal? "list-index"
    (list-index '(a b c) 'b)
    1))
