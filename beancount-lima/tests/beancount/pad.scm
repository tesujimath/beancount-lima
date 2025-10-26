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
                                 (equal? (posting-flag pst) "'P"))))
         (into-list))))
  (check-equal? "pad-postings length" (length current-pad-psts) 1)
  (let ((pad-pst (car current-pad-psts)))
  ;; check each field matches what we expect
    (transduce (list (cons posting-flag "'P")
                   (cons posting-account "Assets:Bank:Current")
                   (cons posting-amount (amount (decimal -22 2) "NZD")))
             (into-for-each (lambda (x) (let ((f (car x))
                                              (expected (cdr x)))
                                    (check-equal? "pad-postings value" (f pad-pst) expected)))))))
