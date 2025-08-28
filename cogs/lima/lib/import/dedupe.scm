(provide make-dedupe-transactions)

;; a filter
(define (make-dedupe-transactions existing-txnids)
  (lambda (txn)
    (let ((txnid (or (hash-try-get txn 'txnid) '())))
      (not (hashset-contains? existing-txnids txnid)))))
