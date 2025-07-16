(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/alist.scm")

(test-module
 "alist-remove tests"
 (check-equal? "alist-remove with repeated keys in alist"
               (alist-remove 'a '((a . 1) (b . 2) (c . 3) (a . 4) (d . 5)))
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

(test-module
 "alist-insert-or-replace tests"
 (check-equal? "alist-insert-or-replace first"
               (alist-insert-or-replace 'a 11 '((a . 1) (b . 2) (c . 3)))
               '((a . 11) (b . 2) (c . 3)))
 (check-equal? "alist-insert-or-replace second"
               (alist-insert-or-replace 'b 102 '((a . 1) (b . 2) (c . 3)))
               '((b . 102) (a . 1) (c . 3)))
 (check-equal? "alist-insert-or-replace not present"
               (alist-insert-or-replace 'd 25 '((a . 1) (b . 2) (c . 3)))
               '((d . 25) (a . 1) (b . 2) (c . 3)))
 (check-equal? "alist-insert-or-replace multiple present"
               (alist-insert-or-replace 'b 27 '((a . 1) (b . 2) (c . 3) (b . 4)))
               '((b . 27) (a . 1) (c . 3))))

(test-module
 "alist-merge tests"
 (check-equal? "alist-merge simple"
               (alist-merge '((a . 1) (b . 2) (c . 3)) '((d . 4) (e . 5)))
               '((e . 5) (d . 4) (a . 1) (b . 2) (c . 3)))
 (check-equal? "alist-merge override"
               (alist-merge '((a . 1) (b . 2) (c . 3)) '((d . 4) (a . 5)))
               '((a . 5) (d . 4) (b . 2) (c . 3)))
 (check-equal? "alist-merge empty0"
               (alist-merge '() '((d . 4) (e . 5)))
               '((e . 5) (d . 4)))
 (check-equal? "alist-merge empty1"
               (alist-merge '((a . 1) (b . 2) (c . 3)) '())
               '((a . 1) (b . 2) (c . 3))))
