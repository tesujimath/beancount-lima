(require "lima/prelude.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let* ((current-psts (account-postings (hash-get (ledger-accounts *ledger*) "Assets:Bank:Current")))
       (pad-psts (filter (make-posting-flagged-with? "P") current-psts)))
  (check-equal? "pad-postings" pad-psts
    (list (posting (date 2024 1 1) (amount (decimal -22 2) "NZD") "P"))))
