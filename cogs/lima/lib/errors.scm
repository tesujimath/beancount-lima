(provide error)

(define (error spanned-element message)
  (list `(message . ,message)
    (alist-get 'element-type spanned-element)
    (alist-get 'span spanned-element)))
