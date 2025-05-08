(provide
  ledger-filter-accounts-by-name)

(require "lima/types.scm")
(require "lima/account.scm")
(require "lima/posting.scm")
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

;; return a new ledger filtered by account name `predicate`
(define (ledger-filter-accounts-by-name predicate ldg)
  (let* ((account-names (filter predicate (ledger-account-names ldg)))
         (accounts (transduce account-names
                    (mapping (lambda (name) (cons name (hash-get (ledger-accounts ldg) name))))
                    (into-hashmap))))
    (ledger
      (ledger-currencies ldg)
      account-names
      accounts)))

;; return a new ledger filtered by posting `predicate`
(define (ledger-filter-postings predicate ldg)
  (let ((accounts (transduce (ledger-account-names ldg)
                   (mapping (lambda (name)
                             (let* ((acc (hash-get (ledger-accounts ldg) name))
                                    (filtered-acc (account-filter-postings predicate acc)))
                               (cons name filtered-acc)))
                     (into-hashmap)))))
    ;; TODO filter out now empty accounts
    (ledger
      (ledger-currencies ldg)
      ;; TODO construct account names from what accounts we have left,
      ; and maybe always do this rather than on Rust side
      (ledger-account-names ldg)
      accounts)))

; (test-module
;   "ledger tests"
;   ;; TODO
;    )
