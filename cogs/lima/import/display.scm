(provide format-transaction format-balance)

(require "srfi/srfi-28/format.scm")
(require "lima/types.scm")
(require "lima/string.scm")
(require "lima/alist.scm")
(require "lima/import/types.scm")

(define (quoted space value)
  (format "~a\"~a\"" space value))

(define (format-amount amt space)
  (format "~a~a~a"
    (amount-number amt)
    space
    (amount-currency amt)))

(define (format-transaction txn txn-directive)
  (let ((space " ")
        (pad "  ")
        (indent "  ")
        (format-secondary-account
          (lambda (acc)
            (if (list? acc)
              (let* ((acc-name (car acc))
                     (n (cadr acc))
                     (suffix (if (> n 1) "s" ""))
                     (category (caddr acc)))
                (format "~a  ; inferred from ~a ~a~a" acc-name n category suffix))
              acc))))
    (format "~a~a~a~a\n~a~a~a~a\n~a\n"
      (cdr-assoc 'date txn)
      space
      txn-directive
      (let ((payee (cdr-assoc-or-default 'payee "" txn))
            (narration (cdr-assoc-or-default 'narration "" txn)))
        (cond
          [(and (string-empty? payee) (string-empty? narration)) ""]
          [(string-empty? payee) (quoted space narration)]
          [else (format "~a~a" (quoted space payee) (quoted space narration))]))
      indent
      (cdr-assoc 'primary-account txn)
      pad
      (format-amount (cdr-assoc 'amount txn) space)
      (foldl (lambda (acc s) (format "~a~a~a\n" s indent (format-secondary-account acc))) ""
        (cdr-assoc-or-default 'secondary-accounts '() txn)))))

(define (format-balance bln account)
  (let ((space " ") (pad "  "))
    (format "~a~abalance~a~a~a~a\n\n"
      (cdr-assoc 'date bln)
      space
      space
      account
      pad
      (format-amount (cdr-assoc 'amount bln) space))))
