;; .dir-locals.el
((scheme-mode
  ;; Geiser doesn't support Steel
  . ((eval . (geiser-mode -1))
     (scheme-program-name . "lima"))))
