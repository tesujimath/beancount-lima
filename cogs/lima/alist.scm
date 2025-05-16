(provide del-assoc cdr-assoc cdr-assoc-or-default cdr-assoc-or-empty alist-symbol-keys?)

(require (only-in "lima/list.scm" all))

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (cdr-assoc-or-default key default alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) default)))

(define (cdr-assoc-or-empty key alist)
  (cdr-assoc-or-default key '() alist))

(define (cdr-assoc key alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) (error! "key not found in alist" key))))

;; do we have an alist with all keys being symbols?
(define (alist-symbol-keys? alist)
  (and (list? alist)
    (all (lambda (item) (and (pair? item) (symbol? (car item))))
      alist)))
