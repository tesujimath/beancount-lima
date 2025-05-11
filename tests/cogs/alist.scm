(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/alist.scm")

(test-module
  "alist tests"
  (check-equal? "del-assoc with repeated keys in alist"
    (del-assoc 'a '((a . 1) (b . 2) (c . 3) (a . 4) (d . 5)))
    '((b . 2) (c . 3) (d . 5))))
