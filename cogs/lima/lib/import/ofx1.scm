(provide make-extract-txn extract-balance)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/types.scm")
(require "lima/lib/stdlib.scm")
(require "lima/lib/import/extract.scm")

;; extract balance from header if we can find the fields we need, otherwise return empty
(define (extract-balance accounts-by-id source)
  (let* ((hdr (import-source-header source))
         (cur (hash-get hdr 'curdef))
         (acctid (or (hash-try-get hdr 'acctid) "unknown-acctid"))
         (account (or (hash-try-get accounts-by-id acctid) "Assets:Unknown"))
         (balamt (or (hash-try-get hdr 'balamt) '()))
         (dtasof (or (hash-try-get hdr 'dtasof) '())))
    (if (or (empty? balamt) (empty? dtasof))
      '()
      ;; Beancount balance date is as of midnight at the beginning of the day, but we have the end of the day, so add 1 day
      (let ((date (date-after (parse-date dtasof "%Y%m%d") 1))
            (amt (parse-decimal-cents balamt)))
        (list (hash 'date date
                    'account account
                    'amount (amount amt cur)))))))

;; extract imported OFX1 transactions into an intermediate representation
(define (make-extract-txn accounts-by-id source)
  (let* ((hdr (import-source-header source))
         (cur (hash-get hdr 'curdef))
         (acctid (or (hash-try-get hdr 'acctid) "unknown-acctid"))
         (primary-account (or (hash-try-get accounts-by-id acctid) "Assets:Unknown"))
         (field-names (import-source-fields source))
         (get-dtposted (make-field-getter field-names "dtposted" (lambda (x) (parse-date x "%Y%m%d"))))
         (get-trnamt (make-field-getter field-names "trnamt" parse-decimal-cents))
         (get-fitid (make-field-getter field-names "fitid" identity))
         (get-name (make-field-getter field-names "name" identity))
         (get-memo (make-field-getter field-names "memo" identity)))
    (lambda (txn)
      (hash 'date (get-dtposted txn)
            'payee (get-name txn)
            'narration (get-memo txn)
            'amount (amount (get-trnamt txn) cur)
            'primary-account primary-account
            'txnid (format "~a.~a" acctid (get-fitid txn))))))
