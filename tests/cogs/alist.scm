(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/alist.scm")

(test-module
  "del-assoc tests"
  (check-equal? "del-assoc with repeated keys in alist"
    (del-assoc 'a '((a . 1) (b . 2) (c . 3) (a . 4) (d . 5)))
    '((b . 2) (c . 3) (d . 5))))

(test-module
  "alist-symbol-keys? tests"
  (check-equal? "alist-symbol-keys? int values"
    (alist-symbol-keys? '((a . 1) (b . 2) (c . 3) (a . 4) (d . 5)))
    #t)
  (check-equal? "alist-symbol-keys? misc values"
    (alist-symbol-keys? '((a . (x y z)) (b . (1 2 3))))
    #t)
  (check-equal? "alist-symbol-keys? string key"
    (alist-symbol-keys? '((a . 1) ("b" . 2)))
    #f)
  (check-equal? "alist-symbol-keys? simple list"
    (alist-symbol-keys? '(a b c))
    #f)
  (check-equal? "alist-symbol-keys? string"
    (alist-symbol-keys? "oops")
    #f)
  (check-equal? "alist-symbol-keys? empty"
    (alist-symbol-keys? '())
    #t))
