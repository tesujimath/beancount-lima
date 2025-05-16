(provide make-field-getter)

(require "lima/types.scm")
(require "lima/list.scm")
(require "lima/import/prelude.scm")
(require "lima/import/account-inference.scm")

(define (make-field-getter field-names name parse)
  (let ((idx (list-index field-names name)))
    (lambda (field-values) (let ((value (list-ref field-values idx)))
                            (parse value)))))
