(require "lima/lib/base-config.scm")
(require "lima/lib/import/globals.scm")
(require "lima/lib/import/importer.scm")

(let* ((import-config (config-value-or-default '(import) '() *config*)))
  (import import-config '() *import-group*))
