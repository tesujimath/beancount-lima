(provide postings->account
  make-subaccount?
  account-filter-postings
  account-currencies)

(require "lima/alist.scm")
(require "lima/types.scm")
(require "lima/posting.scm")

;; for tests
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))
(require "steel/sorting/merge-sort.scm")

(define (add-posting p inv-alist)
  (let* ((amt (posting-amount p))
         (cur (amount-currency amt))
         (cur-total (cdr-assoc-or-default cur (decimal-zero) inv-alist)))
    (cons (cons cur (decimal-add cur-total (amount-number amt)))
      (del-assoc cur inv-alist))))

(define (postings->inv-alist postings)
  (let ((inv-alist (foldl add-posting '() postings)))
    inv-alist))

(define (postings->account postings)
  (account (postings->inv-alist postings) postings))

;; return a predicate which checks whether an account name is a non-strict sub-account of `acc-name`,
;; where non-strict here means an account is a subaccount of itself.
(define (make-subaccount? acc) (lambda (subacc)
                                (or (string=? subacc acc)
                                  (starts-with? subacc (string-append acc ":")))))

;; return a new account filtered by posting `predicate`
(define (account-filter-postings predicate acc)
  (postings->account (filter predicate (account-postings acc))))

;; return account currencies in arbitrary order
(define (account-currencies acc)
  (map car (account-inventory acc)))

(test-module
  "account tests"
  (check-equal? "make-subaccount?" ((make-subaccount? "Bank") "Assets:Bank") #f)
  (check-equal? "make-subaccount?" ((make-subaccount? "Assets") "Assets:Bank") #t)
  (check-equal? "make-subaccount?" ((make-subaccount? "Asset") "Assets:Bank") #f)
  (check-equal? "make-subaccount?" ((make-subaccount? "Assets:Bank") "Assets:Bank") #t)
  (check-equal? "make-subaccount?" ((make-subaccount? "Assets:Bank:Current") "Assets:Bank") #f)

  (check-equal? "account-filter-postings" (account-filter-postings (make-posting-within? (period (date 2025 1 1) (date 2025 2 1)))
                                           (postings->account (list
                                                               (posting (date 2024 12 24) (amount (decimal 750 2) "NZD"))
                                                               (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD"))
                                                               (posting (date 2025 2 19) (amount (decimal 150 2) "NZD")))))
    (postings->account (list
                        (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD")))))
  (check-equal? "account-currencies" (merge-sort (account-currencies
                                                  (postings->account (list
                                                                      (posting (date 2024 12 24) (amount (decimal 750 2) "NZD"))
                                                                      (posting (date 2025 1 5) (amount (decimal 1000 2) "GBP"))
                                                                      (posting (date 2025 2 19) (amount (decimal 150 2) "NZD")))))
                                      #:comparator
                                      string<?)
    '("GBP" "NZD")))
