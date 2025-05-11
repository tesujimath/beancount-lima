(provide imported
  imported-raw-transaction-fields
  imported-raw-transactions
  transaction
  transaction-date
  transaction-payee
  transaction-narration
  transaction-amount
  transaction-base-account
  transaction-other-accounts)

(struct imported (raw-transaction-fields raw-transactions) #:transparent)

(struct transaction (date payee narration amount base-account other-accounts) #:transparent)
