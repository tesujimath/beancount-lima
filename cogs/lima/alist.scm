(provide
  del-assoc
  cdr-assoc
  cdr-assoc-or-default
  cdr-assoc-or-empty
  try-cdr-assoc
  alist-symbol-keys?
  alist-insert-or-replace
  alist-insert
  alist-merge)

(require (only-in "lima/list.scm" all))

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (cdr-assoc-or-default key default alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) default)))

(define (cdr-assoc-or-empty key alist)
  (cdr-assoc-or-default key '() alist))

;; return #f if not found
(define (try-cdr-assoc key alist)
  (cdr-assoc-or-default key #f alist))

(define (cdr-assoc key alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) (error! "key" key "not found in alist" alist))))

;; do we have an alist with all keys being symbols?
(define (alist-symbol-keys? alist)
  (and (list? alist)
    (all (lambda (item) (and (pair? item) (symbol? (car item))))
      alist)))

;; insert or replace an item in an alist
(define (alist-insert-or-replace key value alist)
  (cons (cons key value) (del-assoc key alist)))

;; insert an item in an alist
(define (alist-insert key value alist)
  (cons (cons key value) alist))

;; shallow merge of alists, with the rightmost taking precedence
(define (alist-merge alist0 alist1)
  (foldl (lambda (item1 merged) (alist-insert-or-replace (car item1) (cdr item1) merged)) alist0 alist1))
