(require "lima/base-config.scm")
(require "lima/import/globals.scm")
(require "lima/import/importer.scm")

(let* ((import-config (config-value-or-default '(import) '() *config*)))
  (import import-config '() *import-group*))
