(provide make-field-getter)

(require "lima/lib/list.scm")

(define (make-field-getter field-names name parse)
  (let ((idx (list-index field-names name)))
    (lambda (field-values) (let ((value (list-ref field-values idx)))
                            (parse value)))))
