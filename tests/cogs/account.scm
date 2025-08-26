(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/account.scm")
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
 (check-equal? "make-subaccount? 9" ((make-subaccount? "Assets" "Expenses" "Income") "Liabilities:Credit-Card") #f))
