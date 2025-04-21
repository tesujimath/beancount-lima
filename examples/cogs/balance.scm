(provide balance)

(define balance (lambda ()
                 (map
                   (lambda (an) (cons an (hash-get (ledger-accounts *ledger*) an)))
                   (ledger-account-names *ledger*))))
