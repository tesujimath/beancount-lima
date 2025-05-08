(provide postings->account
  make-account-name-contains?
  account-filter-postings)

(require "lima/alist.scm")
(require "lima/types.scm")
(require "lima/posting.scm")
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

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

;; return a predicate which checks whether an account name contains the string `s`
(define (make-account-name-contains? s) (lambda (name) (string-contains? name s)))

;; return a new account filtered by posting `predicate`
(define (account-filter-postings predicate acc)
  (postings->account (filter predicate (account-postings acc))))

(test-module
  "account tests"
  (check-equal? "make-account-name-contains?" #t ((make-account-name-contains? "Bank") "Assets:Bank"))
  (check-equal? "make-account-name-contains? not" #f ((make-account-name-contains? "Banko") "Assets:Bank"))

  (check-equal? "account-filter-postings" (account-filter-postings (make-posting-within? (period (date 2025 1 1) (date 2025 2 1)))
                                           (postings->account (list
                                                               (posting (date 2024 12 24) (amount (decimal 750 2) "NZD"))
                                                               (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD"))
                                                               (posting (date 2025 2 19) (amount (decimal 150 2) "NZD")))))
    (postings->account (list
                        (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD"))))))
