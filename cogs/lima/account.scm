(provide postings->inv-alist)

(require "lima/alist.scm")
(require "lima/types.scm")

(define (add-posting p inv-alist)
  (let* ((amt (posting-amount p))
         (cur (amount-currency amt))
         (cur-total (cdr-assoc-or-default cur (decimal-zero) inv-alist)))
    (cons (cons cur (decimal-add cur-total (amount-number amt)))
      (del-assoc cur inv-alist))))

(define (postings->inv-alist postings)
  (let ((inv-alist (foldl add-posting '() postings)))
    inv-alist))
