(provide import)

(require "steel/sorting/merge-sort.scm")
(require "lima/lib/list.scm")
(require "lima/lib/base-config.scm")
(require "lima/lib/import/dedupe.scm")
(require "lima/lib/import/account-inference.scm")
(require "lima/lib/import/pairing.scm")
(require "lima/lib/import/format.scm")

;; default extractors:
(require (prefix-in ofx1/ (only-in "lima/lib/import/ofx1.scm")))

;; helper for insert-by-date, where we try pairing for all offsets up to pairing-window-days
;; before falling back to insert as new
(define (insert-by-date-with-limit h txn j-base offset pairing-window-days)
  (let* ((j (+ j-base offset))
         (paired (try-pair txn (or (hash-try-get h j) '()))))
    (if paired
        (hash-insert h j paired)
        (if (> offset 0)
            (insert-by-date-with-limit h txn j-base (- offset) pairing-window-days)
            (let ((next-offset (+ 1 (abs offset))))
              (if (<= next-offset pairing-window-days)
                  (insert-by-date-with-limit h txn j-base next-offset pairing-window-days)
                  (hash-insert h j-base (cons txn (or (hash-try-get h j-base) '())))))))))

;; insert a transaction into the hash-by-date, trying to pair where we can
;; transactions are prepended for their date, so to preserve the order, reverse on extraction
(define (make-insert-by-date pairing-window-days)
  (lambda (h txn)
    (let* ((j (date-julian (hash-get txn 'date))))
      (insert-by-date-with-limit h txn j 0 pairing-window-days))))

(define (all-by-date h)
  (transduce (merge-sort (hash-keys->list h))
             (mapping (lambda (j) (reverse (hash-get h j))))
             (flattening)
             ;; TODO can we do better than into-list here?
             (into-list)))

;; import using the supplied config, which is a hashmap with the following optional keys:
                                        ; accounts - hashmap of account-id to account-name
                                        ; txnid-key - metadata key used for transaction IDs
                                        ; txn-directive - the directive written out for a transaction
(define (import config args imp)
  (let*
      (
                                        ; config
       (import-config (config-value-or-default '(import) (hash) config))
       (extractors-by-path (config-value-or-default '(extractors) (hash) import-config))
       (accounts-by-id (config-value-or-default '(accounts) (hash) import-config))
       (txnid-key (config-value-or-default '(txnid-key) "txnid" import-config))
       (payee2-key (config-value-or-default '(payee2-key) "payee2" import-config))
       (narration2-key (config-value-or-default '(narration2-key) "narration2" import-config))
       (txn-directive (config-value-or-default '(txn-directive) "txn" import-config))
       (indent (config-value-or-default '(indent) 4 import-config))
       (pairing-window-days (config-value-or-default '(pairing-window-days) 0 import-config))
       (comment-column (config-value-or-default '(comment-column) 40 import-config))
       (cost-column (config-value-or-default '(cost-column) 76 import-config))

                                        ; args
       (standalone (hash-try-get args 'standalone))

                                        ; defaults
       (extractors-by-format (hash "ofx1" (hash 'txn ofx1/make-extract-txn
                                                'bal ofx1/extract-balance)))

                                        ; context
       (context (import-context imp))
       (payees (import-context-payees context))
       (narrations (import-context-narrations context))
       (existing-txnids (import-context-txnids context))

                                        ; sources
       (sources (import-sources imp))
       (txns-by-date (transduce sources
                                (mapping (lambda (source)
                                           (let* ((hdr (import-source-header source))
                                                  (path (hash-get hdr 'path))
                                                  (format (hash-get hdr 'format))
                                                  (extractor-by-path (find-and-map-or-default
                                                                      (lambda (k) (string-contains? path k))
                                                                      (hash-keys->list extractors-by-path)
                                                                      (lambda (k) (hash-get extractors-by-path k))
                                                                      #f))
                                                  (extractor (or extractor-by-path
                                                                 (or (hash-try-get extractors-by-format format)
                                                                     (error! "no extractor defined for" format))))
                                                  (txns (import-source-transactions source)))
                                             (with-handler (lambda (err)
                                                             (error! err "when importing" path))
                                                           (transduce txns
                                                                      (mapping ((hash-get extractor 'txn) accounts-by-id source))
                                                                      (filtering (make-dedupe-transactions existing-txnids))
                                                                      (mapping (make-infer-secondary-accounts-from-payees-and-narrations payees narrations))
                                                                      (extending ((hash-get extractor 'bal) accounts-by-id source))
                                                                      (into-list))))))
                                (flattening)
                                (into-reducer (make-insert-by-date pairing-window-days) (hash)))))
    (if standalone
        (display (format-include (import-context-path context))))
    (transduce (all-by-date txns-by-date)
               (into-for-each (lambda (directive) (display
                                                   (if (hash-contains? directive 'primary-account)
                                                       (format-transaction
                                                        directive
                                                        txn-directive
                                                        #:txnid-key
                                                        txnid-key
                                                        #:payee2-key
                                                        payee2-key
                                                        #:narration2-key
                                                        narration2-key
                                                        #:indent
                                                        indent
                                                        #:comment-column
                                                        comment-column
                                                        #:cost-column
                                                        cost-column)
                                                       (format-balance
                                                        directive
                                                        #:cost-column
                                                        cost-column))))))))

;; (bln (extract-balance hdr))) ; TODO balance

;; TODO balance
                                        ; (unless (empty? bln)
                                        ;   (display (format-balance bln primary-account)))
;;
