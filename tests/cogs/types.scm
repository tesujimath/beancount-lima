(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/types.scm")

(test-module
 "date tests"
 (check-equal? "parse date"
               (parse-date "8/2/2025" "%-d/%-m/%Y")
               (date 2025 2 8))

 (check-equal? "make-period-within-previous? 0"
               ((make-period-within-previous? (date 2025 2 8) 0) (date 2025 2 8))
               #f)
 (check-equal? "make-period-within-previous? 1"
               ((make-period-within-previous? (date 2025 2 8) 1) (date 2025 2 8))
               #t)
 (check-equal? "make-period-within-previous? 2 true"
               ((make-period-within-previous? (date 2025 2 8) 2) (date 2025 2 7))
               #t)
 (check-equal? "make-period-within-previous? 2 false"
               ((make-period-within-previous? (date 2025 2 8) 2) (date 2025 2 6))
               #f)
 )
