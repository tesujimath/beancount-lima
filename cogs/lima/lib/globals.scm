(provide *ledger*)

(require "lima/lib/ledger.scm")

(define *ledger* (directives->ledger *directives*))
