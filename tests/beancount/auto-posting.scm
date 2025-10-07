(require "lima/lib/prelude.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let* ((cum (cumulate-postings *directives*))
       (current-nzd (or (hash-try-get
                         (inventory-units (cumulator-account cum "Assets:Bank:Current"))
                         "NZD")
                        (decimal-zero))))
  (check-equal? "current-nzd" current-nzd (decimal -2965 2)))
