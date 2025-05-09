(provide
  accounts->ledger
  ledger-filter)

(require "lima/types.scm")
(require "lima/account.scm")
(require "lima/posting.scm")
(require "steel/sorting/merge-sort.scm")

;; for tests
(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

;; return a ledger from a hashmap of accounts
(define (accounts->ledger accs)
  (let* ((account-names (merge-sort (hash-keys->list accs) #:comparator string<?))
         (hashset-insert-all (lambda (hs items) (foldl (flip hashset-insert) hs items)))
         (currencies (foldl (lambda (acc all-curs) (hashset-insert-all all-curs (account-currencies acc))) (hashset) (hash-values->list accs))))
    (ledger (merge-sort (hashset->list currencies) #:comparator string<?)
      account-names
      accs)))

;; return a new ledger filtered by account name `predicate`
(define (ledger-filter-accounts-by-name predicate ldg)
  (let* ((account-names (filter predicate (ledger-account-names ldg)))
         (accounts (transduce account-names
                    (mapping (lambda (name) (cons name (hash-get (ledger-accounts ldg) name))))
                    (into-hashmap))))
    (ledger
      (ledger-currencies ldg)
      account-names
      accounts)))

;; return a new ledger filtered by posting `predicate`
(define (ledger-filter-postings predicate ldg)
  (let ((accounts (transduce (ledger-account-names ldg)
                   (mapping (lambda (name)
                             (let* ((acc (hash-get (ledger-accounts ldg) name))
                                    (filtered-acc (account-filter-postings predicate acc)))
                               (cons name filtered-acc)))
                     (into-hashmap)))))
    ;; TODO filter out now empty accounts
    (accounts->ledger accounts)))

;; transducer to extract the combined predicate from a predicates alist
;; example:
;; (define preds (list (cons 'date 1) (cons 'account-name (lambda (s) (string-contains? s "ab"))) (cons 'account-name (lambda (s) (string-contains? s "bc")))))
;; (define preds? (transduce preds (combined-predicate 'account-name) (into-list)))
(define (combined-predicate key preds)
  (lambda (x)
    (transduce preds
      (compose
        (filtering (lambda (pair) (eq? key (car pair))))
        (mapping (lambda (pair) ((cdr pair) x))))
      (into-reducer (lambda (a p) (and a p)) #t))))

;; Return a new ledger filtered by `predicates`, which is an alist with the following keys:
;; `account-name` a predicate that takes an account name
;; `date` a predicate that takes a posting date
;;
;; The result is the intersection of all predicates.
(define (ledger-filter predicates ldg)
  (let* ((filtered-account-name? (combined-predicate 'account-name predicates))
         (filtered-date? (combined-predicate 'date predicates))
         (filtered-posting? (lambda (pst) (filtered-date? (posting-date pst))))

         (filtered-account-names
           (filter filtered-account-name?
             (ledger-account-names ldg)))

         (filtered-accounts
           (transduce filtered-account-names
             (mapping (lambda (name)
                       (let* ((acc (hash-get (ledger-accounts ldg) name))
                              (filtered-acc (postings->account (filter filtered-posting? (account-postings acc)))))
                         (cons name filtered-acc))))
             (into-hashmap))))
    (ledger
      (ledger-currencies ldg)
      filtered-account-names
      filtered-accounts)))

(test-module
  "ledger tests"
  (let ((p1 (list
             (cons 'account-name (make-subaccount? "Assets"))
             (cons 'date (make-period-within? (period (date 2025 1 1) (date 2025 2 1))))
             (cons 'account-name (make-subaccount? "Assets:Bank")))))
    (check-equal? "combined-predicate subaccount 1" ((combined-predicate 'account-name p1) "Assets:Bank")
      #t)
    (check-equal? "combined-predicate subaccount 2 not" ((combined-predicate 'account-name p1) "Assets")
      #f)
    (check-equal? "combined-predicate date 1" ((combined-predicate 'date p1) (date 2025 1 3))
      #t)
    (check-equal? "combined-predicate date 2 not" ((combined-predicate 'date p1) (date 2025 2 2))
      #f)))
