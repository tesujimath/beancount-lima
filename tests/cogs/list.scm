(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/list.scm")

(test-module
  "list tests"
  (check-equal? "list-index"
    (list-index '(a b c) 'b)
    1))
