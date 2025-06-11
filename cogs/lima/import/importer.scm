(provide import)

(require "lima/alist.scm")
(require "lima/base-config.scm")
(require "lima/import/types.scm")
(require "lima/import/dedupe.scm")
(require "lima/import/account-inference.scm")
(require "lima/import/display.scm")

;; import a group, using the supplied config, which is an alist with the following optional keys:
; accounts - alist of account-id to account-name
; txnid-key - metadata key used for transaction IDs
; txn-directive - the directive written out for a transaction
; default-currency - the currency used when none is specified in the import
;
; extractors is an alist by format of extractor for that format
(define (import config extractors group)
  (let*
    ( ; config
      (accounts-by-id (config-value-or-default '(accounts) '() config))
      (txnid-key (config-value-or-default '(txnid-key) "txnid" config))
      (txn-directive (config-value-or-default '(txn-directive) "txn" config))
      (default-currency (config-value-or-default '(default-currency) "CAD" config))
      ; group
      (payees (import-group-payees group))
      (narrations (import-group-narrations group))
      (existing-txnids (import-group-txnids group)) #| (bln (extract-balance default-currency hdr))) ; TODO balance|#)
    (transduce (import-group-sources group)
      (mapping (lambda (source)
                (let* ((hdr (import-source-header source))
                       (format (cdr-assoc 'format hdr))
                       (extractor (cdr-assoc format extractors))
                       (txns (import-source-transactions source)))
                  (transduce txns
                    (mapping (extractor default-currency accounts-by-id source))
                    (filtering (make-dedupe-transactions existing-txnids))
                    (mapping (make-infer-secondary-accounts-from-payees-and-narrations payees narrations))
                    (into-list)))))
      (flattening)
      (into-for-each (lambda (txn) (display (format-transaction txn txn-directive #:txnid-key txnid-key)))))))

;; TODO balance
; (unless (empty? bln)
;   (display (format-balance bln primary-account)))
;;
