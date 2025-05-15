(require "srfi/srfi-28/format.scm")
(require "steel/logging/log.scm")
(require "lima/config.scm")
(require "lima/types.scm")
(require "lima/list.scm")
(require "lima/alist.scm")
(require "lima/import/prelude.scm")
(require "lima/import/types.scm")
(require "lima/import/display.scm")

(define default-currency (config-value-or-default '(import default-currency) "CAD" *config*))

;; extract imported OFX1 transactions into an intermediate representation
(define (make-extract hdr fields acctid primary-account)
  (log/info! "make-extract" primary-account)
  (let* ((cur (cdr-assoc-or-default "curdef" default-currency hdr))
         (dtposted-i (list-index fields "dtposted"))
         (trnamt-i (list-index fields "trnamt"))
         (fitid-i (list-index fields "fitid"))
         (name-i (list-index fields "name"))
         (memo-i (list-index fields "memo")))
    (lambda (txn)
      (let* ((date (parse-date (list-ref txn dtposted-i) "%Y%m%d"))
             (name (list-ref txn name-i))
             (memo (list-ref txn memo-i))
             (fitid (list-ref txn fitid-i))
             (amt (parse-decimal (list-ref txn trnamt-i)))
             (txnid (format "~a.~a" acctid fitid)))
        (list (cons 'date date)
          (cons 'payee name)
          (cons 'narration memo)
          (cons 'amount (amount amt cur))
          (cons 'primary-account primary-account)
          (cons 'txnid txnid))))))

;; extract balance from header if we can find the fields we need, otherwise return empty
(define (extract-balance hdr)
  (let* ((cur (cdr-assoc-or-default "curdef" default-currency hdr))
         (balamt (cdr-assoc-or-default "balamt" '() hdr))
         (dtasof (cdr-assoc-or-default "dtasof" '() hdr)))
    (if (or (empty? balamt) (empty? dtasof))
      '()
      ;; Beancount balance date is as of midnight at the beginning of the day, but we have the end of the day, so add 1 day
      (let ((date (date-after (parse-date dtasof "%Y%m%d") 1))
            (amt (parse-decimal balamt)))
        (list (cons 'date date)
          (cons 'amount (amount amt cur)))))))

;; TODO move to a more general place
;; infer expenses account from payees and narrations we found in the ledger
(define (make-infer-secondary-accounts-from-payees-and-narrations payees narrations)
  (lambda (txn)
    (let* ((amount (amount-number (cdr-assoc 'amount txn)))
           (secondary-accounts
             (cond
               [(decimal>? amount (decimal-zero)) '("Income:Unknown")]
               [(decimal<? amount (decimal-zero))
                 (let* ((found-payee (hash-try-get payees (cdr-assoc 'payee txn)))
                        (found-narration (hash-try-get narrations (cdr-assoc 'narration txn))))
                   (cond
                     [found-payee
                       (begin
                         (log/info! "found payee" found-payee)
                         (hash-keys->list found-payee))]
                     [found-narration
                       (begin
                         (log/info! "found narration" found-narration)
                         (hash-keys->list found-narration))]
                     [else '("Expenses:Unknown")]))]
               [else '()])))
      (cons (cons 'secondary-accounts secondary-accounts) txn))))

(let* ((accounts-by-id (config-value-or-default '(import accounts) '() *config*))
       (hdr (imported-header *imported*))
       (acctid (cdr-assoc-or-default "acctid" "unknown-acctid" hdr))
       (primary-account (cdr-assoc-or-default acctid "Assets:Unknown" accounts-by-id))
       (fields (imported-fields *imported*))
       (txns (imported-transactions *imported*))
       (payees (imported-payees *imported*))
       (narrations (imported-narrations *imported*))
       (bln (extract-balance hdr)))
  (transduce txns
    (mapping (make-extract hdr fields acctid primary-account))
    (mapping (make-infer-secondary-accounts-from-payees-and-narrations payees narrations))
    (into-for-each (lambda (txn) (display (format-transaction txn)))))
  (unless (empty? bln)
    (display-balance bln primary-account)))
