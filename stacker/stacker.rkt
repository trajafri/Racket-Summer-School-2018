#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,(car (foldl handle empty src-datums))))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR)
  #'(#%module-begin
     (display HANDLE-EXPR)))
(provide (rename-out [stacker-module-begin #%module-begin]))

; handle: Expr [ListOf Expr] -> [ListOf Expr]
(define (handle arg acc)
  (match* (arg acc)
    [((? void? arg) _) acc]
    [((? number? arg) _) (cons arg acc)]
    [((? (λ (x) (equal? '+ x)) arg) `(,x ,y . ,xs)) (cons (+ x y) xs)]
    [((? (λ (x) (equal? '* x)) arg) `(,x ,y . ,xs)) (cons (* x y) xs)]
    [(_ _) acc]))

(provide + *)