(provide *base-config* config-value-or-default merge-config)

(define *base-config*
  (hash
    'import
    (hash
      'txn-directive "txn"
      'txnid-key "txnid"
      'txnid2-key "txnid2"
      'pairing-window-days 3
      'comment-column 40
      'indent 2
      'cost-column 76)))

(define (config-value-or-default key-path default cfg)
  (if (empty? key-path) cfg
    (let ((subcfg (hash-try-get cfg (car key-path))))
      (if subcfg (config-value-or-default (cdr key-path) default subcfg) default))))

;; example
;; (config-value-or-default '(import txnid-key) "default-key" *base-config*)

;; merge configs by key, preferring `cfg1`
(define (merge-config cfg0 cfg1)
  (cond [(hash-empty? cfg0) cfg1]
    [(hash-empty? cfg1) cfg0]
    [else
      (let* ((keys0 (transduce (hash-keys->list cfg0) (into-hashset)))
             (keys1 (transduce (hash-keys->list cfg1) (into-hashset)))
             (all-keys (hashset->list (transduce (hash-keys->list cfg0) (extending (hash-keys->list cfg1)) (into-hashset))))
             (common? (lambda (key) (and (hashset-contains? keys0 key) (hashset-contains? keys1 key)))))
        (transduce all-keys
          (mapping (lambda (k)
              (cond [(common? k) (cons k (merge-config (hash-get cfg0 k) (hash-get cfg1 k)))]
                [(hashset-contains? keys0 k) (cons k (hash-get cfg0 k))]
                [else (cons k (hash-get cfg1 k))])))
          (into-hashmap)))]))
