(provide postings->account
  make-subaccount?
  account-filter-postings
  account-currencies)

(require "lima/alist.scm")
(require "lima/types.scm")
(require "lima/posting.scm")

(define (add-posting p inv-alist)
  (let* ((amt (posting-amount p))
         (cur (amount-currency amt))
         (inv-cur-prev-total (cdr-assoc-or-default cur (decimal-zero) inv-alist))
         (inv-cur-new-total (decimal-add inv-cur-prev-total (amount-number amt))))
    (if (decimal-zero? inv-cur-new-total)
      (del-assoc cur inv-alist)
      (alist-insert-or-replace cur inv-cur-new-total inv-alist))))

(define (postings->inv-alist postings)
  (let ((inv-alist (foldl add-posting '() postings)))
    inv-alist))

(define (postings->account postings)
  (account (postings->inv-alist postings) postings))

(define (subaccount? subacc acc)
  (or (string=? subacc acc)
    (starts-with? subacc (string-append acc ":"))))

;; return a predicate which checks whether an account name is a non-strict sub-account of `acc` or any of `rest`,
;; where non-strict here means an account is a subaccount of itself.
(define (make-subaccount? acc0 . rest) (lambda (subacc)
                                        (letrec ((subaccount-any? (lambda (accs)
                                                                   (cond
                                                                     [(empty? accs) #f]
                                                                     [(subaccount? subacc (car accs)) #t]
                                                                     [else (subaccount-any? (cdr accs))]))))
                                          (subaccount-any? (cons acc0 rest)))))

;; return a new account filtered by posting `predicate`
(define (account-filter-postings predicate acc)
  (postings->account (filter predicate (account-postings acc))))

;; return account currencies in arbitrary order
(define (account-currencies acc)
  (map car (account-inventory acc)))
