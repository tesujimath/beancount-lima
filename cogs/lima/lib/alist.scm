(provide
  alist?
  alist-remove
  alist-get
  alist-get-or-default
  alist-get-or-empty
  alist-try-get
  alist-contains?
  alist-symbol-keys?
  alist-insert-or-replace
  alist-insert
  alist-merge)

(require (only-in "lima/lib/list.scm" all))

;; is `x` an alist?
(define (alist? x)
  (and (list? x)
    (all pair? x)))

(define (alist-remove key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (alist-get-or-default key default alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) default)))

(define (alist-get-or-empty key alist)
  (alist-get-or-default key '() alist))

;; return #f if not found
(define (alist-try-get key alist)
  (alist-get-or-default key #f alist))

(define (alist-get key alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) (error! "key" key "not found in alist" alist))))

(define (alist-contains? key alist)
  (and (assoc key alist) #t))

;; do we have an alist with all keys being symbols?
(define (alist-symbol-keys? alist)
  (and (list? alist)
    (all (lambda (item) (and (pair? item) (symbol? (car item))))
      alist)))

;; insert or replace an item in an alist
(define (alist-insert-or-replace key value alist)
  (cons (cons key value) (alist-remove key alist)))

;; insert an item in an alist
(define (alist-insert key value alist)
  (cons (cons key value) alist))

;; shallow merge of alists, with the rightmost taking precedence
(define (alist-merge alist0 alist1)
  (foldl (lambda (item1 merged) (alist-insert-or-replace (car item1) (cdr item1) merged)) alist0 alist1))
