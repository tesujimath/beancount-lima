(provide del-assoc cdr-assoc cdr-assoc-or-default list->alist)

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (cdr-assoc-or-default key default alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) default)))

(define (cdr-assoc key alist)
  (let ((kv (assoc key alist)))
    (if kv (cdr kv) (error! "key not found in alist" key))))

(define (list->alist xs)
  (letrec ((list->alist-acc (lambda (xs pairs)
                             (cond [(empty? xs) pairs]
                               [(empty? (cdr xs)) (error! "odd length list")]
                               [else (list->alist-acc (cddr xs) (cons (cons (car xs) (cadr xs)) pairs))]))))
    (list->alist-acc xs '())))
