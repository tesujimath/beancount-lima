(provide imported
  imported-header
  imported-fields
  imported-transactions
  transaction
  transaction-date
  transaction-payee
  transaction-narration
  transaction-amount
  transaction-base-account
  transaction-other-accounts)

(struct imported (header fields transactions) #:transparent)

(struct transaction (date payee narration amount base-account other-accounts) #:transparent)
