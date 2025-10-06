(provide *inventories*)

(require "lima/lib/inventory.scm")

(define *inventories* (directives->inventories *directives*))
