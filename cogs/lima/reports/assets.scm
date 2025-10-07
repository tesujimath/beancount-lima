(require "lima/lib/prelude.scm")

(let* ((rollup (hash-try-get *args* 'rollup))
       (assets-account-name (hash-get *options* 'name_assets))
       (cum (cumulate-postings *directives* #:filters (hash 'account (lambda (acc) (starts-with? acc assets-account-name))))))
  (if rollup
      (display-rollup cum)
      (display-balance-sheet cum)))
