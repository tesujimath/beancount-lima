(provide *config*)

(require "lima/base-config.scm")

(define *config*
  (merge-config
    *base-config*
    '((import . ((accounts .
                  (("99-1234-0123456-07" . "Assets:Bank:Current")
                   ("99-1234-0123456-21" . "Assets:Bank:Savings"))))))))
