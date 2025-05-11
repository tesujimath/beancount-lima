(provide compose)

(define compose (lambda (f g) (lambda (arg) (f (g arg)))))

;; example
;; (map (compose amount-number posting-amount) (account-postings current-account)
