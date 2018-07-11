#lang racket

(require (for-syntax syntax/parse))

#;(begin-for-syntax
    (define-syntax-class byte
      (pattern n_number #:fail-unless (<= (syntax-e #'n) 256) "bad byte")))

; ----------

; An Expr is one of:
; - (function-application Variable Expr)
; - (if Expr Expr Expr)
; - Variable
; - Number
; - String

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (fun-name:id param:id ...) body:expr) (define arity (length (syntax->list #'(param ...))))
                                              #`(define-syntax fun-name
                                                  (cons #,arity #'(λ (param ...) body)))]))

(define-for-syntax (lookup func-name)
  (define x (syntax-local-value func-name))
  (values (car x) (cdr x)))

(define-syntax (func-app stx)
  (syntax-parse stx
    [(_ rator:id rand:expr ...) #:do ((define-values (arity func) (lookup #'rator)))
                                #:fail-unless (= arity (length (syntax->list #'(rand ...))))
                                "wrong number of arguments"
                                #`(#,func rand ...)]))

(define-function (f x y) (+ x y))
(func-app f 1 2)
