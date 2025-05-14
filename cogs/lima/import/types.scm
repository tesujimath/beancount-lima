(provide imported
  imported-header
  imported-fields
  imported-transactions
  imported-txnids
  imported-payees
  imported-narrations)

(struct imported (header fields transactions txnids payees narrations) #:transparent)
