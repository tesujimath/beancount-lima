(provide *config*)

(require "lima/base-config.scm")

(define *config* *base-config*)

;; or, for example
; (define *config*
;   (merge-config
;     *base-config*
;     '((import . ((default-currency . "CHF"))))))
