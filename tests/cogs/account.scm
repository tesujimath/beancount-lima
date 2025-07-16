(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/account.scm")
(require "lima/lib/posting.scm")
(require "lima/lib/types.scm")
(require "steel/sorting/merge-sort.scm")

(set-test-mode!)
(test-module
 "account tests"
 (check-equal? "make-subaccount? 1" ((make-subaccount? "Bank") "Assets:Bank") #f)
 (check-equal? "make-subaccount? 2" ((make-subaccount? "Assets") "Assets:Bank") #t)
 (check-equal? "make-subaccount? 3" ((make-subaccount? "Asset") "Assets:Bank") #f)
 (check-equal? "make-subaccount? 4" ((make-subaccount? "Assets:Bank") "Assets:Bank") #t)
 (check-equal? "make-subaccount? 5" ((make-subaccount? "Assets:Bank:Current") "Assets:Bank") #f)
 (check-equal? "make-subaccount? 6" ((make-subaccount? "Assets" "Expenses") "Assets:Bank") #t)
 (check-equal? "make-subaccount? 7" ((make-subaccount? "Assets" "Expenses") "Expenses:Bank") #t)
 (check-equal? "make-subaccount? 8" ((make-subaccount? "Assets" "Expenses" "Income") "Income:Work") #t)
 (check-equal? "make-subaccount? 9" ((make-subaccount? "Assets" "Expenses" "Income") "Liabilities:Credit-Card") #f)

 (check-equal? "account-filter-postings" (account-filter-postings (make-posting-within? (period (date 2025 1 1) (date 2025 2 1)))
                                                                  (postings->account (list
                                                                                      (posting (date 2024 12 24) (amount (decimal 750 2) "NZD") '())
                                                                                      (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD") '())
                                                                                      (posting (date 2025 2 19) (amount (decimal 150 2) "NZD") '()))))
               (postings->account (list
                                   (posting (date 2025 1 5) (amount (decimal 1000 2) "NZD") '()))))
 (check-equal? "account-currencies" (merge-sort (account-currencies
                                                 (postings->account (list
                                                                     (posting (date 2024 12 24) (amount (decimal 750 2) "NZD") '())
                                                                     (posting (date 2025 1 5) (amount (decimal 1000 2) "GBP") '())
                                                                     (posting (date 2025 2 19) (amount (decimal 150 2) "NZD") '()))))
                                                #:comparator
                                                string<?)
               '("GBP" "NZD")))
