(require "lima/types.scm")
(provide
  decimal->rational
  ledger
  ledger?
  ledger-currencies
  ledger-account-names
  ledger-accounts
  account
  account?
  account-inventory
  account-postings
  period
  period?
  period-within?
  make-period-within?
  posting
  posting?
  posting-date
  posting-amount
  amount
  amount?
  amount-number
  amount-currency)

(require "lima/tabulate.scm")
(provide tabulate)

(require "lima/ffi.scm")

(provide
  *ledger*)

;; The FFI ledger is called `*ffi-ledger*`.
;; Here we create a native Steel ledger called `*ledger*`.
;
; The reason we convert all FFIValues into native Steel ones is to avoid
; keeping having to pass these across the FFI, as that requires cloning of values
; rather than simply passing references.
(define *ledger* (ffi-ledger->ledger *ffi-ledger*))
