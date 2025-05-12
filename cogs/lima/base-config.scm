(provide *base-config* config-value-or-default)

(define *base-config*
  '((import . ((txnid-key . "txnid")
               (other-import-config . "woohoo")))
    (count . ((something-else . "booyah!")))))

(define (config-value-or-default key-path default cfg)
  (if (empty? key-path) cfg
    (let ((subcfg (assoc (car key-path) cfg)))
      (if subcfg (config-value-or-default (cdr key-path) default (cdr subcfg)) default))))

;; example
;; (config-value-or-default '(import txnid-key) "default-key" *base-config*)
