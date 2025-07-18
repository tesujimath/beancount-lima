(provide import-group
  import-group-sources
  import-group-path
  import-group-txnids
  import-group-payees
  import-group-narrations
  import-source
  import-source-header
  import-source-fields
  import-source-transactions)

(struct import-group (sources path txnids payees narrations) #:transparent)
(struct import-source (header fields transactions) #:transparent)
