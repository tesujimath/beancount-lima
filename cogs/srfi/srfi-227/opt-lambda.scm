;; https://srfi.schemers.org/srfi-227/srfi-227.html
;;
;; COPYRIGHT © 2021 Marc Nieper-Wißkirchen, Daphne Preston-Kendal.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; iot-lambda meta helper library:

(define-syntax rec
  (lambda (stx)
    (syntax-case stx ()
      [(_ f e)
        #'(letrec ([f e]) f)])))

(define make-opt-lambda-transformer
  (lambda (who sequential?)
    (lambda (stx)
      (syntax-case stx ()
        [(_ (formal ... . rest) body1 ... body2)
          (let*-values
            ([(var1* bdg*)
                (let f ([formal* #'(formal ...)])
                  (syntax-case formal* ()
                    [()
                      (values '() '())]
                    [(var formal ...)
                      (identifier? #'var)
                      (let-values ([(var1* bdg*)
                                     (f #'(formal ...))])
                        (values (cons #'var var1*) bdg*))]
                    [_
                      (values '() formal*)]))]
              [(var2* init*)
                (let f ([bdg* bdg*])
                  (syntax-case bdg* ()
                    [()
                      (values '() '())]
                    [([var init] bdg ...)
                      (identifier? #'var)
                      (let-values ([(var2* init*)
                                     (f #'(bdg ...))])
                        (values (cons #'var var2*)
                          (cons #'init init*)))]
                    [_
                      (syntax-violation who
                        "invalid bindings"
                        stx
                        bdg*)]))]
              [(tmp1*) (if sequential?
                        var1*
                        (generate-temporaries var1*))]
              [(tmp2*) (if sequential?
                        var2*
                        (generate-temporaries var2*))])
            #`(rec f
               (case-lambda
                #,@(let f ([tmp1* tmp1*] [var1* var1*]
                           [tmp2* tmp2*]
                           [var2* var2*]
                           [init* init*])
                    (if (null? var2*)
                      (list #`[(#,@var1* . rest) body1 ... body2])
                      (cons #`[(#,@tmp1*)
                               (f #,@tmp1* #,(car init*))]
                        (f (append tmp1* (list (car tmp2*)))
                          (append var1* (list (car var2*)))
                          (cdr tmp2*)
                          (cdr var2*)
                          (cdr init*))))))))]))))

;; opt-lambda library

(define-syntax opt-lambda
  (make-opt-lambda-transformer 'opt-lambda #f))

(define-syntax opt*-lambda
  (make-opt-lambda-transformer 'opt*-lambda #t))

(define-syntax let-optionals
  (syntax-rules ()
    [(_ expr opt-formals body1 ... body2)
      (apply (opt-lambda opt-formals body1 ... body2) expr)]))

(define-syntax let-optionals*
  (syntax-rules ()
    [(_ expr opt-formals body1 ... body2)
      (apply (opt*-lambda opt-formals body1 ... body2) expr)]))

(define-syntax define-optionals
  (syntax-rules ()
    [(_ (name . opt-formals) body1 ... body2)
      (define name (opt-lambda opt-formals body1 ... body2))]))

(define-syntax define-optionals*
  (syntax-rules ()
    [(_ (name . opt-formals) body1 ... body2)
      (define name (opt*-lambda opt-formals body1 ... body2))]))
