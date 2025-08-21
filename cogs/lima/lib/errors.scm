(provide error)

(define (error spanned-element message)
  (list `(message . ,message)
    (assoc 'element-type spanned-element)
    (assoc 'span spanned-element)))
