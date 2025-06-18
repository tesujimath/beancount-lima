(require "steel/tests/unit-test.scm"
  (for-syntax "steel/tests/unit-test.scm"))

(require "lima/lib/base-config.scm")

(test-module
  "base-config tests"
  (let ((config0 '((import . ((txnid-key . "txnid")
                              (other-import-config . "woohoo")))
                   (count . ((something-else . "booyah!"))))))
    (check-equal? "config-value-or-default key path exists"
      (config-value-or-default '(import txnid-key) "default-key" config0)
      "txnid")
    (check-equal? "config-value-or-default key path doesn't exist"
      (config-value-or-default '(import missing) "default-key" config0)
      "default-key")
    (check-equal? "config-value-or-default empty key path"
      (config-value-or-default '() "default-key" config0)
      config0)))
