(provide make-dedupe-transactions)

(require "lima/alist.scm")

;; a filter
(define (make-dedupe-transactions existing-txnids)
  (lambda (txn)
    (let ((txnid (cdr-assoc-or-empty 'txnid txn)))
      (not (hashset-contains? existing-txnids txnid)))))
