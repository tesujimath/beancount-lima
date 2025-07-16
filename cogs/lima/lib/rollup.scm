(provide display-rollup)

(require "steel/sorting/merge-sort.scm")
(require "srfi/srfi-28/format.scm")
(require "lima/lib/types.scm")
(require "lima/lib/alist.scm")
(require "lima/lib/tabulate.scm")

(define (inventory-for-currencies inv currencies)
  (map (lambda (cur)
        (alist-get-or-default cur "" inv))
    currencies))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-rollup
         ldg
         #:cur
         [cur (ledger-main-currency ldg)])
  (let* ((account-names (ledger-account-names ldg))
         (accounts (ledger-accounts ldg))
         (depth-rollup (foldl (lambda (accname depth-rollup)
                                (let* ((depth (car depth-rollup))
                                       (rollup0 (cdr depth-rollup))
                                       (acc (hash-get accounts accname))
                                       (bal (alist-get-or-default cur (decimal-zero) (account-inventory acc)))
                                       (subacns (split-many accname ":"))
                                       (parent-accnames (foldl (lambda (sub accs)
                                                              (let* ((last-parent (car accs))
                                                                     (next-parent (format "~a:~a" last-parent sub)))
                                                                (cons next-parent accs)))
                                                       (list (car subacns))
                                                               (cdr subacns)))
                                       (rollup1 (foldl (lambda (accname rollup)
                                                         (let* ((oldbal (or (hash-try-get rollup accname) (decimal-zero)))
                                                                (newbal (decimal-add oldbal bal)))
                                                           (hash-insert rollup accname newbal)))
                                                       rollup0
                                                       parent-accnames)))
                                  (cons (max depth (length subacns)) rollup1)))
                        (cons 0 (hash))
                              account-names))
         (depth (car depth-rollup))
         (rollup (transduce (cdr depth-rollup)
                            (filtering (lambda (name-bal) (not (decimal-zero? (second name-bal)))))
                            (into-hashmap)))
         (rollup-account-names (merge-sort (transduce rollup
                                                      (mapping first)
                                                      (into-list))
                                           #:comparator string<?))
         (rollup-combined (transduce rollup-account-names
                                     (mapping (lambda (accname) (list accname
                                                                      (hash-get rollup accname)
                                                                      (let ((acc (hash-try-get accounts accname)))
                                                                        (if acc (alist-get-or-default cur (decimal-zero) (account-inventory acc))
                                                                            (decimal-zero))))))
                                     (into-list))))
    rollup-combined))

(define (display-rollup ldg)
  (display (tabulate (collate-rollup ldg) 'left 'centre)))
