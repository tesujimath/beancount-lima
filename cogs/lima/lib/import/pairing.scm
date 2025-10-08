(provide is-pair? try-pair)

(require "srfi/srfi-28/format.scm")
(require "lima/lib/list.scm")

;; not worth including in types
(define (amount-zero-sum? a1 a2)
  (and (equal? (amount-currency a1) (amount-currency a2))
       (decimal-zero? (decimal-add (amount-number a1) (amount-number a2)))))

;; do the transactions comprise a pair, that is, values sum to zero and the accounts match counter-symmetrically
;; note: dates are ignored here, a date threshold should be applied before calling this
;; note that if either transaction has already been paired (has txnid2), it is no longer available
(define (is-pair? txn0 txn1)
  (if (or (hash-contains? txn0 'txnid2) (hash-contains? txn1 'txnid2)) #f
      (let ((a0 (hash-get txn0 'amount))
            (a1 (hash-get txn1 'amount))
            (p0 (hash-try-get txn0 'primary-account))
            (p1 (hash-try-get txn1 'primary-account))
            (s0 (or (hash-try-get txn0 'secondary-accounts) '()))
            (s1 (or (hash-try-get txn1 'secondary-accounts) '()))
            (equal-p-s? (lambda (p s)
                          (and (eq? (length s) 1)
                               (equal? p (hash-get (car s) 'name))))))
        (and (amount-zero-sum? a0 a1)
             (equal-p-s? p0 s1)
             (equal-p-s? p1 s0)))))

;; pair two transactions by returning the first with txnid2 from the second's txnid (if any), otherwise a comment
;; also with payee and narration from the second as additional fields
(define (pair txn0 txn2)
  (let* ((txnid2 (hash-try-get txn2 'txnid))
         (payee2 (hash-try-get txn2 'payee))
         (narration2 (hash-try-get txn2 'narration))
         (with-txnid (if txnid2
                         (hash-insert txn0 'txnid2 txnid2)
                         (let ((comment (format "paired with \"~a\" \"~a\""
                                                (or (hash-try-get txn2 'payee) "")
                                                (or (hash-try-get txn2 'narration) ""))))
                           (hash-insert txn0 'comment comment))))
         (with-payee (if payee2 (hash-insert with-txnid 'payee2 payee2) with-txnid))
         (with-narration (if narration2 (hash-insert with-payee 'narration2 narration2) with-payee)))
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
