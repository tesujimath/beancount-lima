(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/import/pairing.scm")

(test-module
 "is-pair? tests"
 (check-equal? "is-pair? one account"
               (is-pair? (hash 'amount (amount (decimal -350 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (hash 'amount (amount (decimal 350 2) "NZD")
                               'primary-account "B"
                               'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                               'txnid "t1"))
               #t)
 (check-equal? "try-pair different amount"
               (is-pair? (hash 'amount (amount (decimal -450 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (hash 'amount (amount (decimal 350 2) "NZD")
                               'primary-account "B"
                               'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                               'txnid "t1"))
               #f))

(test-module
 "try-pair tests"
 (check-equal? "try-pair one account success"
               (try-pair (hash 'amount (amount (decimal -350 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (list (hash 'amount (amount (decimal 350 2) "NZD")
                                     'primary-account "B"
                                     'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                                     'txnid "t1")))
               (list (hash 'txnid2 "t2"
                           'amount (amount (decimal 350 2) "NZD")
                           'primary-account "B"
                           'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                           'txnid "t1")))
 (check-equal? "try-pair one account success but no txnid"
               (try-pair (hash 'amount (amount (decimal -350 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee")))
                         (list (hash 'amount (amount (decimal 350 2) "NZD")
                                     'primary-account "B"
                                     'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                                     'txnid "t1")))
               (list (hash 'comment "paired with \"\" \"\""
                           'amount (amount (decimal 350 2) "NZD")
                           'primary-account "B"
                           'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                           'txnid "t1")))
 (check-equal? "try-pair amount mismatch"
               (try-pair (hash 'amount (amount (decimal -450 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (list (hash 'amount (amount (decimal 350 2) "NZD")
                                     'primary-account "B"
                                     'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                                     'txnid "t1")))
               #f)
 (check-equal? "try-pair account mismatch"
               (try-pair (hash 'amount (amount (decimal -450 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "C" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (list (hash 'amount (amount (decimal 350 2) "NZD")
                                     'primary-account "B"
                                     'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                                     'txnid "t1")))
               #f)
 (check-equal? "try-pair multiple accounts"
               (try-pair (hash 'amount (amount (decimal -450 2) "NZD")
                               'primary-account "A"
                               'secondary-accounts (list (hash 'name "B" 'infer-count 1 'infer-category "payee")
                                                         (hash 'name "C" 'infer-count 1 'infer-category "payee"))
                               'txnid "t2")
                         (list (hash 'amount (amount (decimal 350 2) "NZD")
                                     'primary-account "B"
                                     'secondary-accounts (list (hash 'name "A" 'infer-count 1 'infer-category "payee"))
                                     'txnid "t1")))
               #f))
