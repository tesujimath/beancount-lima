(provide compose flatmap)

(define compose (lambda (f g) (lambda (arg) (f (g arg)))))
:; example
;; (map (compose amount-number posting-amount) (account-postings current-account)

(define (flatmap proc seq)
  (letrec ((accumulate
             (lambda (op initial sequence)
               (if (null? sequence)
                 initial
                 (op (car sequence)
                   (accumulate op initial (cdr sequence)))))))
    (accumulate append '() (map proc seq))))

;; example
;; (flatmap (lambda (x) (list 'freddy x)) '(1 2 3))
;; => '(freddy 1 freddy 2 freddy 3)
