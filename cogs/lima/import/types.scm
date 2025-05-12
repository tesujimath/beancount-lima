(provide imported
  imported-transaction-fields
  imported-transactions
  transaction
  transaction-date
  transaction-payee
  transaction-narration
  transaction-amount
  transaction-base-account
  transaction-other-accounts)

(struct imported (transaction-fields transactions) #:transparent)

(struct transaction (date payee narration amount base-account other-accounts) #:transparent)
