(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(provide del-assoc cdr-assoc-or-default)

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (cdr-assoc-or-default key default alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) default)))

(test-module
  "alist tests"
  (check-equal? "del-assoc with repeated keys in alist"
    (del-assoc 'a '((a . 1) (b . 2) (c . 3) (a . 4) (d . 5)))
    '((b . 2) (c . 3) (d . 5))))
