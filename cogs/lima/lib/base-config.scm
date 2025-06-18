(provide *base-config* config-value-or-default merge-config)
(require "lima/lib/alist.scm")

(define *base-config*
  '((import . ((txn-directive . "txn")
               (txnid-key . "txnid")
               (txnid2-key . "txnid2")
               (pairing-window-days . 3)))
    (count . ((something-else . "booyah!")))))

(define (config-value-or-default key-path default cfg)
  (if (empty? key-path) cfg
    (let ((subcfg (assoc (car key-path) cfg)))
      (if subcfg (config-value-or-default (cdr key-path) default (cdr subcfg)) default))))

;; example
;; (config-value-or-default '(import txnid-key) "default-key" *base-config*)

;; merge configs by key, preferring `cfg1`
(define (merge-config cfg0 cfg1)
  (cond [(empty? cfg0) cfg1]
    [(empty? cfg1) cfg0]
    [(and (alist-symbol-keys? cfg0) (alist-symbol-keys? cfg1))
      (let* ((keys0 (transduce cfg0 (mapping car) (into-hashset)))
             (keys1 (transduce cfg1 (mapping car) (into-hashset)))
             (all-keys (hashset->list (transduce cfg0 (extending cfg1) (mapping car) (into-hashset))))
             (common? (lambda (key) (and (hashset-contains? keys0 key) (hashset-contains? keys1 key)))))
        (map (lambda (k)
              (cond [(common? k) (cons k (merge-config (cdr (assoc k cfg0)) (cdr (assoc k cfg1))))]
                [(hashset-contains? keys0 k) (assoc k cfg0)]
                [else (assoc k cfg1)]))
          all-keys))]
    [else cfg1]))
