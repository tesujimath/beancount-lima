(provide *config*)

(require "lima/lib/base-config.scm")

(define *config*
  (merge-config
    *base-config*
    (hash 'ledger "examples/beancount/simple.beancount"
      'import
      (hash 'accounts (hash "99-1234-0123456-07" "Assets:Bank:Current"
                            "99-1234-0123456-21" "Assets:Bank:Savings"
                            "10-9999-0000001-01" "Assets:Bank:Uk:Current"
                            "10-9999-0000001-02" "Assets:Bank:Uk:Savings")))))
