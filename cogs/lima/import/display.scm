(provide format-transaction display-transactions)

(require "srfi/srfi-28/format.scm")
(require "lima/types.scm")
(require "lima/string.scm")
(require "lima/alist.scm")
(require "lima/import/types.scm")

(define (quoted space value)
  (format "~a\"~a\"" space value))

(define (format-transaction txn accounts)
  (let ((space " ") (pad "  ") (indent "  "))
    (format "~a~atxn~a\n~a~a~a~a\n~a\n"
      (cdr-assoc 'date txn)
      space
      (let ((payee (cdr-assoc-or-default 'payee "" txn))
            (narration (cdr-assoc-or-default 'narration "" txn)))
        (cond
          [(and (string-empty? payee) (string-empty? narration)) ""]
          [(string-empty? payee) (quoted space narration)]
          [else (format "~a~a" (quoted space payee) (quoted space narration))]))
      indent
      (car accounts)
      pad
      (let ((amt (cdr-assoc 'amount txn)))
        (format "~a~a~a"
          (amount-number amt)
          space
          (amount-currency amt)))
      (foldl (lambda (acc-name s) (format "~a~a~a\n" s indent acc-name)) "" (cdr accounts)))))

(define (display-transactions txns)
  (for-each (lambda (txn)
             (display (format-transaction txn)))
    txns))
