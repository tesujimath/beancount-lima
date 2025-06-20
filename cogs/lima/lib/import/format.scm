(provide format-transaction format-balance)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/types.scm")
(require "lima/lib/string.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/import/types.scm")
(require "lima/lib/import/account-inference.scm")

(define (quoted space value)
  (format "~a\"~a\"" space value))

(define (format-amount amt space)
  (format "~a~a~a"
    (amount-number amt)
    space
    (amount-currency amt)))

(define (format-transaction
         txn
         txn-directive
         #:txnid-key
         [txnid-key "txnid"]
         #:txnid2-key
         [txnid2-key "txnid2"]
         #:payee2-key
         [payee2-key "payee2"]
         #:narration2-key
         [narration2-key "narration2"])
  (let ((space " ")
        (pad "  ")
        (indent "  ")
        (acc-width 60))
    (format "~a~a~a~a\n~a~a~a~a~a~a~a~a~a\n~a\n"
      (alist-get 'date txn)
      space
      txn-directive
      (let ((payee (alist-get-or-default 'payee "" txn))
            (narration (alist-get-or-default 'narration "" txn)))
        (cond
          [(and (string-empty? payee) (string-empty? narration)) ""]
          [(string-empty? payee) (quoted space narration)]
          [else (format "~a~a" (quoted space payee) (quoted space narration))]))
      (let ((comment (alist-get-or-empty 'comment txn)))
        (if (empty? comment) ""
          (format "~a; ~a\n" indent comment)))
      (let ((txnid (alist-get-or-empty 'txnid txn)))
        (if (empty? txnid) ""
          (format "~a~a: \"~a\"\n" indent txnid-key txnid)))
      (let ((txnid2 (alist-get-or-empty 'txnid2 txn)))
        (if (empty? txnid2) ""
          (format "~a~a: \"~a\"\n" indent txnid2-key txnid2)))
      (let ((payee2 (alist-get-or-empty 'payee2 txn)))
        (if (empty? payee2) ""
          (format "~a~a: \"~a\"\n" indent payee2-key payee2)))
      (let ((narration2 (alist-get-or-empty 'narration2 txn)))
        (if (empty? narration2) ""
          (format "~a~a: \"~a\"\n" indent narration2-key narration2)))
      indent
      (alist-get 'primary-account txn)
      pad
      (format-amount (alist-get 'amount txn) space)
      (foldl (lambda (acc s) (format "~a~a~a\n" s indent (format-secondary-account acc acc-width))) ""
        (alist-get-or-empty 'secondary-accounts txn)))))

(define (format-balance bln)
  (let ((space " ") (pad "  "))
    (format "~a~abalance~a~a~a~a\n\n"
      (alist-get 'date bln)
      space
      space
      (alist-get 'account bln)
      pad
      (format-amount (alist-get 'amount bln) space))))
