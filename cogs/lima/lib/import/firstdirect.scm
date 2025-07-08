(provide extractors)

(require "lima/lib/types.scm")
(require "lima/lib/list.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/stdlib.scm")
(require "lima/lib/import/extract.scm")
(require "lima/lib/import/types.scm")
(require "lima/lib/import/importer.scm")

(define (extract-balance cur)
  (lambda (accounts-by-id source)
    (let* ((hdr (import-source-header source))
           (path (alist-get 'path hdr))
           (account (find-and-map-or-default
                     (lambda (account-lookup) (string-contains? path (car account-lookup)))
                     accounts-by-id
                     cdr
                     "Assets:Unknown"))
           (field-names (import-source-fields source))
           (txns (import-source-transactions source))
           (get-balance (make-field-getter field-names "balance" parse-decimal))
           ;; Beancount balance date is as of midnight at the beginning of the day, but we have the end of the day, so add 1 day
           (get-date (make-field-getter field-names "date" (lambda (x) (date-after (parse-date x "%d/%m/%Y") 1))))
           (txn0 (car txns)))
      (list `((date . ,(get-date txn0))
              (amount . ,(amount (get-balance txn0) cur))
              (account . ,account))))))

;; extract imported First Direct flavour CSV transactions into an intermediate representation
;; where the account is inferred from the path by picking the first in `accounts-by-id`
;; which is contained in the import path
(define (make-extract-txn cur)
  (lambda (accounts-by-id source)
    (let* ((hdr (import-source-header source))
           (path (alist-get 'path hdr))
           (primary-account (find-and-map-or-default
                             (lambda (account-lookup) (string-contains? path (car account-lookup)))
                             accounts-by-id
                             cdr
                             "Assets:Unknown"))
           (field-names (import-source-fields source))
           (get-date (make-field-getter field-names "date" (lambda (x) (parse-date x "%d/%m/%Y"))))
           (get-amount (make-field-getter field-names "amount" parse-decimal))
           (get-description (make-field-getter field-names "description" identity)))
      (lambda (txn)
        `((date . ,(get-date txn))
          (amount . ,(amount (get-amount txn) cur))
          (primary-account . ,primary-account)
          (narration . ,(get-description txn)))))))

(define extractors
  ;; First Direct is a UK bank, so set currency accordingly
  (let ((cur "GBP"))
    `((txn . ,(make-extract-txn cur))
      (bal . ,(extract-balance cur)))))
