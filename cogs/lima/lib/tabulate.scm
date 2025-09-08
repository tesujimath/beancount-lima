(provide tabulate spaces)

;; a column may contain strings or decimals or both
;; string-width is the max string length,
;; decimal-left is the max number of digits and/or sign to left of decimal point
;; decimal-right is the max number of digits after the decimal point.
(struct cellspec (string-width decimal-left decimal-right) #:transparent)

(define (cellspec-from-cell cell)
  (cond
   [(string? cell)
    (cellspec (string-length cell) 0 0)]
   [(decimal? cell)
    (cellspec 0
              (decimal-width-left cell)
              (decimal-width-right cell))]
   [else (error! (cons cell "cell is not string or decimal"))]))

(define (cellspec-add cs1 cs2)
  (cellspec (max (cellspec-string-width cs1) (cellspec-string-width cs2))
            (max (cellspec-decimal-left cs1) (cellspec-decimal-left cs2))
            (max (cellspec-decimal-right cs1) (cellspec-decimal-right cs2))))

(define (cellspec-add-cell cs cell)
  (cellspec-add cs (cellspec-from-cell cell)))

;; align a cellspec so that the string width and decimal widths line up
(define (cellspec-aligned cs)
  (let ((decimal-width (+ 1 (cellspec-decimal-left cs) (cellspec-decimal-right cs))))
    (if (> decimal-width (cellspec-string-width cs))
        (cellspec decimal-width (cellspec-decimal-left cs) (cellspec-decimal-right cs))
        (cellspec (cellspec-string-width cs)
                  (+ (cellspec-decimal-left cs) (- (cellspec-string-width cs) decimal-width))
                  (cellspec-decimal-right cs)))))

(define (rowspec-add-row-acc acc row rs)
  (cond [(and (null? rs) (null? row)) acc]
        [(null? row) (rowspec-add-row-acc (cons (car rs) acc) '() (cdr rs))]
        [else (let ((rs (if (null? rs) (list (cellspec 0 0 0)) rs)))
                (rowspec-add-row-acc (cons (cellspec-add-cell (car rs) (car row)) acc) (cdr row) (cdr rs)))]))

(define (rowspec-add-row row rs)
  (reverse (rowspec-add-row-acc '() row rs)))

(define (rowspec-from-rows rows)
  (foldl rowspec-add-row '() rows))

(define (repeat-string n s)
  (transduce (map (lambda (i) s) (range n)) (into-string)))

(define (spaces n) (repeat-string n " "))

(define (display-spaces n) (display (spaces n)))

(define (display-cell just cs cell)
  (cond
   [(string? cell)
                                        ; left/right/centre-aligned according to just
    (let* ((pad (- (cellspec-string-width cs) (string-length cell)))
           (pad-left-right
            (cond
             [(eq? just 'left) (cons 0 pad)]
             [(eq? just 'right) (cons pad 0)]
             [(or (eq? just 'centre) (eq? just 'center)) (cons (quotient pad 2) (- pad (quotient pad 2)))]
             [else (error! "tabulate: found justification" just " expected one of 'left 'right 'centre 'center")])))
      (display-spaces (car pad-left-right))
      (display cell)
      (display-spaces (cdr pad-left-right)))]
   [(decimal? cell)
                                        ; always decimal-point aligned
    (let ((pad-left (- (cellspec-decimal-left cs) (decimal-width-left cell)))
          (pad-right (- (cellspec-decimal-right cs) (decimal-width-right cell))))
      (display-spaces pad-left)
      (display (decimal->string cell))
      (display-spaces pad-right))]
   [else (error! (cons cell "cell is not string or decimal"))]))

;; we may assume rs is at least as long as row, because it was derived from all the rows
(define (make-display-row just inter-column-pad rs)
  (lambda (row)
    (letrec ((display-row-with-padding (lambda (just pad rs row)
                                         (if (null? row) (displayln)
                                             (begin
                                               (display pad)
                                               (display-cell (car just) (car rs) (car row))
                                               (display-row-with-padding
                                        ; repeat lasy element of just
                                                (if (null? (cdr just)) just (cdr just))
                                                inter-column-pad
                                                (cdr rs)
                                                (cdr row)))))))
      (display-row-with-padding just "" rs row))))

;; rows is a list of lists of strings and/or decimals
;; strings are left/right/centre justified as specified with default of left, and decimals are point-justified
(define (tabulate rows . just)
  (let* ((rs (rowspec-from-rows rows))
         (aligned-rs (map cellspec-aligned rs)))
    (for-each (make-display-row (if (null? just) (list 'left) just) "  " aligned-rs) rows)))
