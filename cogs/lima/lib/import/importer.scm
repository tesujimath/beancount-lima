(provide import)

(require "steel/sorting/merge-sort.scm")
(require "lima/lib/list.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/base-config.scm")
(require "lima/lib/import/types.scm")
(require "lima/lib/import/dedupe.scm")
(require "lima/lib/import/account-inference.scm")
(require "lima/lib/import/pairing.scm")
(require "lima/lib/import/format.scm")

;; default extractors:
(require (prefix-in ofx1/ (only-in "lima/lib/import/ofx1.scm")))

;; insert a transaction into the hash-by-date, trying to pair where we can
;; TODO check other dates up to pairing-window-days
(define (insert-by-date h txn)
  (let* ((j (date-julian (alist-get 'date txn)))
         (existing-txns-for-date (or (hash-try-get h j) '())))
    (hash-insert h j (or (try-pair txn existing-txns-for-date)
                      (cons txn existing-txns-for-date)))))

(define (all-by-date h)
  (transduce (merge-sort (hash-keys->list h))
    (mapping (lambda (j) (hash-get h j)))
    (flattening)
    ;; TODO can we do better than into-list here?
    (into-list)))

;; import a group, using the supplied config, which is an alist with the following optional keys:
; accounts - alist of account-id to account-name
; txnid-key - metadata key used for transaction IDs
; txn-directive - the directive written out for a transaction
(define (import config group)
  (let*
    (
      ; config
      (import-config (config-value-or-default '(import) '() config))
      (extractors-by-path (config-value-or-default '(extractors) '() import-config))
      (accounts-by-id (config-value-or-default '(accounts) '() import-config))
      (txnid-key (config-value-or-default '(txnid-key) "txnid" import-config))
      (payee2-key (config-value-or-default '(payee2-key) "payee2" import-config))
      (narration2-key (config-value-or-default '(narration2-key) "narration2" import-config))
      (txn-directive (config-value-or-default '(txn-directive) "txn" import-config))

      ; defaults
      (extractors-by-format `(("ofx1" . ((txn . ,ofx1/make-extract-txn)
                                         (bal . ,ofx1/extract-balance)))))

      ; group
      (payees (import-group-payees group))
      (narrations (import-group-narrations group))
      (existing-txnids (import-group-txnids group))
      (txns-by-date (transduce (import-group-sources group)
                     (mapping (lambda (source)
                               (let* ((hdr (import-source-header source))
                                      (path (alist-get 'path hdr))
                                      (format (alist-get 'format hdr))
                                      (extractor-by-path (find-and-map-or-default
                                                          (lambda (extractor-lookup) (string-contains? path (car extractor-lookup)))
                                                          extractors-by-path
                                                          cdr
                                                          #f))
                                      (extractor (cond
                                                  [extractor-by-path extractor-by-path]
                                                  [(alist-contains? format extractors-by-format)
                                                    (alist-get format extractors-by-format)]
                                                  [else (error! "no extractor defined for" format)]))
                                      (txns (import-source-transactions source)))
                                 (with-handler (lambda (err)
                                                (error! err "when importing" path))
                                   (transduce txns
                                     (mapping ((alist-get 'txn extractor) accounts-by-id source))
                                     (filtering (make-dedupe-transactions existing-txnids))
                                     (mapping (make-infer-secondary-accounts-from-payees-and-narrations payees narrations))
                                     (extending ((alist-get 'bal extractor) accounts-by-id source))
                                     (into-list))))))
                     (flattening)
                     (into-reducer insert-by-date (hash)))))
    (transduce (all-by-date txns-by-date)
      (into-for-each (lambda (directive) (display
                                          (if (alist-contains? 'primary-account directive)
                                            (format-transaction
                                              directive
                                              txn-directive
                                              #:txnid-key
                                              txnid-key
                                              #:payee2-key
                                              payee2-key
                                              #:narration2-key
                                              narration2-key)
                                            (format-balance directive))))))))

;; (bln (extract-balance hdr))) ; TODO balance

;; TODO balance
; (unless (empty? bln)
;   (display (format-balance bln primary-account)))
;;
