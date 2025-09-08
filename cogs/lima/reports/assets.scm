(require "lima/lib/prelude.scm")

(let* ((rollup (hash-try-get *args* 'rollup))
       (assets-account-name (hash-get *options* 'name_assets))
       (assets (ledger-filter `((account-name . ,(lambda (s) (starts-with? s assets-account-name)))) *ledger*)))
  (if rollup
      (display-rollup assets)
      (display-balance-sheet assets)))
