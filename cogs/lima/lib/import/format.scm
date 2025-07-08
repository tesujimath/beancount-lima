(provide format-transaction format-balance)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/types.scm")
(require "lima/lib/string.scm")
(require "lima/lib/alist.scm")
(require (only-in "lima/lib/tabulate.scm" spaces))
(require "lima/lib/import/types.scm")
(require "lima/lib/import/account-inference.scm")

(define (quoted space value)
  (format "~a\"~a\"" space value))

(define (format-amount amt space)
  (format "~a~a~a"
    (amount-number amt)
    space
    (amount-currency amt)))

(define (format-amount-in-column prefix cost-column amt space suffix)
  (let
    ((amount-width-left (decimal-width-left (amount-number amt))))
    (format "~a~a~a~a"
      prefix
      (spaces (max 2 (- cost-column (+ (string-length prefix) amount-width-left))))
      (format-amount amt space)
      suffix)))

;; format the inferred secondary account name with a comment about where it came from
(define (format-secondary-account prefix comment-column acc suffix)
  (let* ((name (alist-get 'name acc))
         (count (alist-get-or-empty 'infer-count acc))
         (category (alist-get-or-empty 'infer-category acc))
         (no-inference (or (empty? count) (empty? category))))
    (format "~a~a~a"
      prefix
      (if no-inference
        name
        (let ((plural (if (> count 1) "s" "")))
          (format "~a~a; inferred from ~a ~a~a"
            name
            (spaces (max 1 (- comment-column (+ (string-length prefix) (string-length name)))))
            count
            category
            plural)))
      suffix)))

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
         [narration2-key "narration2"]
         #:comment-column
         [comment-column 40]
         #:cost-column
         [cost-column 76])
  (let* ((space " ")
         (pad "  ")
         (indent "  ")
         (date (alist-get 'date txn))
         (payee (alist-get-or-default 'payee "" txn))
         (narration (alist-get-or-default 'narration "" txn))
         (comment (alist-get-or-empty 'comment txn))
         (txnid (alist-get-or-empty 'txnid txn))
         (txnid2 (alist-get-or-empty 'txnid2 txn))
         (payee2 (alist-get-or-empty 'payee2 txn))
         (narration2 (alist-get-or-empty 'narration2 txn))
         (primary-account (alist-get 'primary-account txn))
         (amt (alist-get 'amount txn))
         (secondary-accounts (alist-get-or-empty 'secondary-accounts txn)))
    (format "~a~a~a~a\n~a~a~a~a~a~a\n~a\n"
      date
      space
      txn-directive
      (cond
        [(and (string-empty? payee) (string-empty? narration)) ""]
        [(string-empty? payee) (quoted space narration)]
        [else (format "~a~a" (quoted space payee) (quoted space narration))])
      (if (empty? comment) ""
        (format "~a; ~a\n" indent comment))
      (if (empty? txnid) ""
        (format "~a~a: \"~a\"\n" indent txnid-key txnid))
      (if (empty? txnid2) ""
        (format "~a~a: \"~a\"\n" indent txnid2-key txnid2))
      (if (empty? payee2) ""
        (format "~a~a: \"~a\"\n" indent payee2-key payee2))
      (if (empty? narration2) ""
        (format "~a~a: \"~a\"\n" indent narration2-key narration2))
      (format-amount-in-column
        (format "~a~a" indent primary-account)
        cost-column
        amt
        space
        "")
      (foldl (lambda (acc s) (format "~a~a" s (format-secondary-account indent comment-column acc "\n"))) ""
        secondary-accounts))))

(define (format-balance bln
         #:cost-column
         [cost-column 76])
  (let ((space " ")
        (pad "  ")
        (date (alist-get 'date bln))
        (account (alist-get 'account bln))
        (amt (alist-get 'amount bln)))
    (format-amount-in-column
      (format "~a~abalance~a~a"
        date
        space
        space
        account)
      cost-column
      amt
      space
      "\n\n")))
