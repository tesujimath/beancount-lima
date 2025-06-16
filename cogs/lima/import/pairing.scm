(provide is-pair? try-pair)

(require "srfi/srfi-28/format.scm")
(require "lima/types.scm")
(require "lima/alist.scm")
(require "lima/list.scm")

;; not worth including in types
(define (amount-zero-sum? a1 a2)
  (and (equal? (amount-currency a1) (amount-currency a2))
    (decimal-zero? (decimal-add (amount-number a1) (amount-number a2)))))

;; do the transactions comprise a pair, that is, values sum to zero and the accounts match counter-symmetrically
;; note: dates are ignored here, a date threshold should be applied before calling this
;; note that if either transaction has already been paired (has txnid2), it is no longer available
(define (is-pair? txn0 txn1)
  (if (or (alist-contains? 'txnid2 txn0) (alist-contains? 'txnid2 txn1)) #f
    (let ((a0 (cdr-assoc 'amount txn0))
          (a1 (cdr-assoc 'amount txn1))
          (p0 (try-cdr-assoc 'primary-account txn0))
          (p1 (try-cdr-assoc 'primary-account txn1))
          (s0 (cdr-assoc-or-empty 'secondary-accounts txn0))
          (s1 (cdr-assoc-or-empty 'secondary-accounts txn1))
          (equal-p-s? (lambda (p s)
                       (and (eq? (length s) 1)
                         (equal? p (cdr-assoc 'name (car s)))))))
      (and (amount-zero-sum? a0 a1)
        (equal-p-s? p0 s1)
        (equal-p-s? p1 s0)))))

;; pair two transactions by returning the first with txnid2 from the second's txnid (if any), otherwise a comment
;; also with payee and narration from the second as additional fields
(define (pair txn0 txn2)
  (let* ((txnid2 (try-cdr-assoc 'txnid txn2))
         (payee2 (try-cdr-assoc 'payee txn2))
         (narration2 (try-cdr-assoc 'narration txn2))
         (with-txnid (if txnid2
                      (alist-insert 'txnid2 txnid2 txn0)
                      (let ((comment (format "paired with \"~a\" \"~a\""
                                      (cdr-assoc-or-default 'payee "" txn2)
                                      (cdr-assoc-or-default 'narration "" txn2))))
                        (alist-insert 'comment comment txn0))))
         (with-payee (if payee2 (alist-insert 'payee2 payee2 with-txnid) with-txnid))
         (with-narration (if narration2 (alist-insert 'narration2 narration2 with-payee) with-payee)))
    with-narration))

;; try pairing a transaction into a list of txns, returning new list on success, or #f on fail
(define (try-pair txn2 txns)
  (let* ((paired-unpaired (partition (lambda (txn) (is-pair? txn2 txn)) txns))
         (paired (car paired-unpaired))
         (unpaired (cdr paired-unpaired)))
    (if (empty? paired)
      #f
      (append
        (cons (pair (car paired) txn2) (cdr paired))
        unpaired))))
