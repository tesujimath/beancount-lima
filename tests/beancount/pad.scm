(require "lima/lib/prelude.scm")
(require (for-syntax "steel/tests/unit-test.scm"))

(let* ((current-pad-psts
       (transduce
         *directives*
         (filtering transaction?)
         (mapping transaction-postings)
         (flattening)
         (filtering (lambda (pst)
                            (and (equal? (posting-account pst) "Assets:Bank:Current")
                                 (equal? (posting-flag pst) "P"))))
         (into-list))))
  (check-equal? "pad-postings" current-pad-psts
    (list (posting "P" "Assets:Bank:Current" (amount (decimal -22 2) "NZD")))))
