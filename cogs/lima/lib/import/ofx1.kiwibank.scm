(provide extractors)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/string.scm")
(require (prefix-in ofx1/  "lima/lib/import/ofx1.scm"))

;; vanilla ofx1
(define extract-balance ofx1/extract-balance)

;; Alas Kiwibank replicate the payee into the narration, in different ways:
;; -
;; modified by removing payee prefix from narration if it contains a semi-colon
(define (make-extract-txn accounts-by-id source)
  (let ((extract (ofx1/make-extract-txn accounts-by-id source)))
    (lambda (txn)
      (let* ((extracted (extract txn))
             (payee (hash-get extracted 'payee))
             (narration (hash-get extracted 'narration))
             (narration-with-prefix (split-once narration ";"))
             (equal-truncated? (lambda (truncated untruncated limit)
                                 (equal? truncated (substring untruncated 0 (min (string-length untruncated) limit)))))
             (payee-limit 32))
        (cond [(and (list? narration-with-prefix) (> (length narration-with-prefix) 1))
               (let ((prefix  (first narration-with-prefix))
                     (narration-proper (second narration-with-prefix)))
                 (if (or (starts-with? prefix payee)
                         ;; also consider without POS W/D prefix
                         (starts-with? (string-remove-prefix prefix "POS W/D ") payee))
                     (hash-insert extracted 'narration narration-proper)
                     extracted))]
              ;; if the narration is simply the untruncated version of the payee, then replace the payee with it
              [(equal-truncated? payee narration payee-limit)
               (hash-insert (hash-remove extracted 'narration)
                            'payee narration)]
              [else extracted])))))

(define extractors
  (hash 'txn make-extract-txn
        'bal extract-balance))
