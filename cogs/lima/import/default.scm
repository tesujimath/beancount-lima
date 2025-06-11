(require "lima/base-config.scm")
(require "lima/import/globals.scm")
(require "lima/import/importer.scm")
(require (only-in "lima/import/ofx1.scm" [make-extract ofx1-make-extract]))

(displayln "Default importer, just OFX1 for now")

(let* ((import-config (config-value-or-default '(import) '() *config*)))
  (import import-config `(("ofx1" . ,ofx1-make-extract)) *import-group*))
