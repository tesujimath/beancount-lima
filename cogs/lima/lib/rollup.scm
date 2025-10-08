(provide display-rollup)

(require "steel/sorting/merge-sort.scm")
(require "srfi/srfi-28/format.scm")

(define (repeated value n) (letrec ((repeated_ (lambda (value n a) (if (<= n 0) a (repeated_ value (- n 1) (cons value a))))))
                             (repeated_ value n '())))

;; make a single rollup row, ready for tabulation
(define (rollup-row accname rollup-bal rollup-merged bal depth max-depth)
  (cons accname (if rollup-merged
                    (append (repeated "" (- depth 1))
                            (list (if (decimal-zero? rollup-bal) "" rollup-bal))
                            (repeated "" (- max-depth depth 1))
                            (list (if (decimal-zero? bal) "" bal)))
                    (append (repeated "" (- max-depth 1))
                            (list (if (decimal-zero? bal) "" bal)))
                    )))

(define (rollup cum cur)
  (foldl (lambda (accname0 depth-rollup)
           (let* ((depth (first depth-rollup))
                  (rollup0 (second depth-rollup))
                  (inv0 (inventory-units (cumulator-account cum accname0)))
                  (subacns (split-many accname0 ":"))
                  (parent-accnames0 (foldl (lambda (sub accs)
                                             (let* ((last-parent (car accs))
                                                    (next-parent (format "~a:~a" last-parent sub)))
                                               (cons next-parent accs)))
                                           (list (car subacns))
                                           (cdr subacns)))
                  (bal0 (or (hash-try-get inv0 cur) (decimal-zero)))
                  (rollup1 (if (decimal-zero? bal0)
                               rollup0
                               (foldl (lambda (accname1 rollup)
                                        (let* ((old-bal-merged (or (hash-try-get rollup accname1) (list (decimal-zero) #f)))
                                               (old-bal (first old-bal-merged))
                                               (old-merged (second old-bal-merged))
                                               (new-bal (decimal-add old-bal bal0))
                                               (new-merged (or old-merged (not (equal? accname1 accname0)))))
                                          (hash-insert rollup accname1 (list new-bal new-merged))))
                                      rollup0
                                      parent-accnames0))))
             (list (max depth (length subacns)) rollup1)))
         (list 0 (hash))
         (cumulator-account-names cum)))

;; collate means build a list of rows with columns ready for tabulation
(define (collate-rollup
         cum
         #:cur
         [cur (cumulator-main-currency cum)])
  (let* ((account-names (cumulator-account-names cum))
         (depth-rollup (rollup cum cur))
         (max-depth (first depth-rollup))
         (rollup-accounts (transduce (second depth-rollup)
                                     (filtering (lambda (name-bal) (not (decimal-zero? (first (second name-bal))))))
                                     (into-hashmap)))
         (rollup-account-names (merge-sort (transduce rollup-accounts
                                                      (mapping first)
                                                      (into-list))
                                           #:comparator string<?))
         (rollup-combined (transduce rollup-account-names
                                     (mapping (lambda (accname0)
                                                (let* ((depth (length (split-many accname0 ":")))
                                                       (rollup-bal-merged (hash-get rollup-accounts accname0))
                                                       (rollup-bal (first rollup-bal-merged))
                                                       (rollup-merged (second rollup-bal-merged))
                                                       (bal (let ((inv (cumulator-account cum accname0)))
                                                              (if inv (or (hash-try-get (inventory-units inv) cur) (decimal-zero))
                                                                  (decimal-zero)))))
                                                  (rollup-row accname0
                                                              rollup-bal
                                                              rollup-merged
                                                              bal
                                                              depth
                                                              max-depth))))
                                     (into-list))))
    rollup-combined))

(define/contract
  (display-rollup cum)
  (->/c cumulator? void?)
  (display (tabulate (collate-rollup cum))))
