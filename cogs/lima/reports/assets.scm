(require "lima/lib/prelude.scm")

(let* ((rollup (hash-try-get *args* 'rollup))
       (assets-account-name (hash-get *options* 'name_assets))
       (assets (inventories-filter `((account-name . ,(lambda (s) (starts-with? s assets-account-name)))) *inventories*)))
  (if rollup
      (display-rollup assets)
      (display-balance-sheet assets)))
