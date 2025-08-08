(require "lima/lib/prelude.scm")

(let* ((rollup (alist-get-or-default 'rollup #f *cli-options*))
      (assets-account-name (alist-get 'name_assets (ledger-options *ledger*)))
      (assets (ledger-filter `((account-name . ,(lambda (s) (starts-with? s assets-account-name)))) *ledger*)))
  (if rollup
    (display-rollup assets)
    (display-balances assets)))
