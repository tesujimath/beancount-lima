(require "lima/types.scm")
(require "lima/list.scm")
(require "lima/alist.scm")
(require "lima/stdlib.scm")
(require "lima/import/prelude.scm")
(require "lima/import/extract.scm")
(require "lima/import/account-inference.scm")
(require "lima/import/importer.scm")
(require "lima/import/globals.scm")
(require "lima/base-config.scm")

(define (extract-balance cur)
  (lambda (accounts-by-id source)
    (let* ((hdr (import-source-header source))
           (path (cdr-assoc 'path hdr))
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
(define (make-extract cur)
  (lambda (accounts-by-id source)
    (let* ((hdr (import-source-header source))
           (path (cdr-assoc 'path hdr))
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

;; First Direct is a UK bank, so set currency accordingly
(let ((cur "GBP"))
  (import (config-value-or-default '(import) '() *config*)
    `(("csv" . ((txn . ,(make-extract cur))
                (bal . ,(extract-balance cur)))))
    *import-group*))
