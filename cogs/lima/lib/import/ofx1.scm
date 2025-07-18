(provide make-extract-txn extract-balance)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/types.scm")
(require "lima/lib/list.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/stdlib.scm")
(require "lima/lib/import/extract.scm")
(require "lima/lib/import/types.scm")

;; extract balance from header if we can find the fields we need, otherwise return empty
(define (extract-balance accounts-by-id source)
  (let* ((hdr (import-source-header source))
         (cur (alist-get 'curdef hdr))
         (acctid (alist-get-or-default 'acctid "unknown-acctid" hdr))
         (account (alist-get-or-default acctid "Assets:Unknown" accounts-by-id))
         (balamt (alist-get-or-empty 'balamt hdr))
         (dtasof (alist-get-or-empty 'dtasof hdr)))
    (if (or (empty? balamt) (empty? dtasof))
      '()
      ;; Beancount balance date is as of midnight at the beginning of the day, but we have the end of the day, so add 1 day
      (let ((date (date-after (parse-date dtasof "%Y%m%d") 1))
            (amt (parse-decimal-cents balamt)))
        (list `((date . ,date)
                (account . ,account)
                (amount . ,(amount amt cur))))))))

;; extract imported OFX1 transactions into an intermediate representation
(define (make-extract-txn accounts-by-id source)
  (let* ((hdr (import-source-header source))
         (cur (alist-get 'curdef hdr))
         (acctid (alist-get-or-default 'acctid "unknown-acctid" hdr))
         (primary-account (alist-get-or-default acctid "Assets:Unknown" accounts-by-id))
         (field-names (import-source-fields source))
         (get-dtposted (make-field-getter field-names "dtposted" (lambda (x) (parse-date x "%Y%m%d"))))
         (get-trnamt (make-field-getter field-names "trnamt" parse-decimal-cents))
         (get-fitid (make-field-getter field-names "fitid" identity))
         (get-name (make-field-getter field-names "name" identity))
         (get-memo (make-field-getter field-names "memo" identity)))
    (lambda (txn)
      `((date . ,(get-dtposted txn))
        (payee . ,(get-name txn))
        (narration . ,(get-memo txn))
        (amount . ,(amount (get-trnamt txn) cur))
        (primary-account . ,primary-account)
        (txnid . ,(format "~a.~a" acctid (get-fitid txn)))))))
