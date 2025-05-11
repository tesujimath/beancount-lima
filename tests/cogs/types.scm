(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/types.scm")

(test-module
  "date tests"
  (check-equal? "parse date"
    (parse-date "8/2/2025" "%-d/%-m/%Y")
    (date 2025 2 8)))
