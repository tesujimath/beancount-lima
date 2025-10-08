(provide format-transaction format-balance format-include)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/string.scm")
(require "lima/lib/import/account-inference.scm")

(define (quoted space value)
  (format "~a\"~a\"" space value))

(define (repeat-string n s)
  (transduce (map (lambda (i) s) (range n)) (into-string)))

(define (spaces n) (repeat-string n " "))

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
  (let* ((name (hash-get acc 'name))
         (count (or (hash-try-get acc 'infer-count) '()))
         (category (or (hash-try-get acc 'infer-category) '()))
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
         #:indent
         [indent 4]
         #:comment-column
         [comment-column 40]
         #:cost-column
         [cost-column 76])
  (let* ((space " ")
         (date (hash-get  txn'date))
         (payee (or (hash-try-get txn 'payee) ""))
         (narration (or (hash-try-get txn 'narration) ""))
         (comment (or (hash-try-get txn 'comment) '()))
         (txnid (or (hash-try-get txn 'txnid) '()))
         (txnid2 (or (hash-try-get txn 'txnid2) '()))
         (payee2 (or (hash-try-get txn 'payee2) '()))
         (narration2 (or (hash-try-get txn 'narration2) '()))
         (primary-account (hash-get txn 'primary-account))
         (amt (hash-get txn 'amount))
         (secondary-accounts (or (hash-try-get txn 'secondary-accounts) '())))
    (format "~a~a~a~a\n~a~a~a~a~a~a\n~a\n"
            date
            space
            txn-directive
            (cond
             [(and (string-empty? payee) (string-empty? narration)) ""]
             [(string-empty? payee) (quoted space narration)]
             [else (format "~a~a" (quoted space payee) (quoted space narration))])
            (if (empty? comment) ""
                (format "~a; ~a\n" (spaces indent) comment))
            (if (empty? txnid) ""
                (format "~a~a: \"~a\"\n" (spaces indent) txnid-key txnid))
            (if (empty? txnid2) ""
                (format "~a~a: \"~a\"\n" (spaces indent) txnid2-key txnid2))
            (if (or (empty? payee2) (string-empty? payee2)) ""
                (format "~a~a: \"~a\"\n" (spaces indent) payee2-key payee2))
            (if (or (empty? narration2) (string-empty? narration2)) ""
                (format "~a~a: \"~a\"\n" (spaces indent) narration2-key narration2))
            (format-amount-in-column
             (format "~a~a" (spaces indent) primary-account)
             cost-column
             amt
             space
             "")
            (foldl (lambda (acc s) (format "~a~a" s (format-secondary-account (spaces indent) comment-column acc "\n"))) ""
                   secondary-accounts))))

(define (format-balance bln
                        #:cost-column
                        [cost-column 76])
  (let ((space " ")
        (date (hash-get bln 'date))
        (account (hash-get bln 'account))
        (amt (hash-get bln 'amount)))
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

(define (format-include path)
  (format "include \"~a\"\n\n" path))
