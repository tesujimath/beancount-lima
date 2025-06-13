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

(define (extract-balance cur field-names txns)
  (let* ((get-balance (make-field-getter field-names "balance" parse-decimal))
         ;; Beancount balance date is as of midnight at the beginning of the day, but we have the end of the day, so add 1 day
         (get-date (make-field-getter field-names "date" (lambda (x) (date-after (parse-date x "%d/%m/%Y") 1))))
         (txn0 (car txns)))
    (list (cons 'date (get-date txn0))
      (cons 'amount (amount (get-balance txn0) cur)))))

;; extract imported First Direct flavour CSV transactions into an intermediate representation
;; where the account is inferred from the path by picking the first in `accounts-by-id`
;; which is contained in the import path
(define (make-extract default-currency accounts-by-id source)
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
      (list (cons 'date (get-date txn))
        (cons 'amount (amount (get-amount txn) default-currency))
        (cons 'primary-account primary-account)
        (cons 'narration (get-description txn))))))

;; make this usable from CLI
(let* ((import-config (config-value-or-default '(import) '() *config*)))
  ;; First Direct is a UK bank, so set default currency accordingly
  (import (alist-merge import-config '((default-currency . "GBP")))
    `(("csv" . ,make-extract))
    *import-group*))
