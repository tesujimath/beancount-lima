(provide
  make-subaccount?)

(define (subaccount? subacc acc)
  (or (string=? subacc acc)
    (starts-with? subacc (string-append acc ":"))))

;; return a predicate which checks whether an account name is a non-strict sub-account of `acc` or any of `rest`,
;; where non-strict here means an account is a subaccount of itself.
(define (make-subaccount? acc0 . rest) (lambda (subacc)
                                        (letrec ((subaccount-any? (lambda (accs)
                                                                   (cond
                                                                     [(empty? accs) #f]
                                                                     [(subaccount? subacc (car accs)) #t]
                                                                     [else (subaccount-any? (cdr accs))]))))
                                          (subaccount-any? (cons acc0 rest)))))
