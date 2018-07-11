#lang racket
(require (for-syntax syntax/parse)
         rackunit)

(provide require #%module-begin #%top-interaction app
         plus minus mult string+ smallest biggest div
         both any gt lt eq str-eq iszero ++ -- str-len
         then else
         (rename-out
          [literal #%datum]
          [define-function define]
          [iff if]))

(module reader syntax/module-reader
  algebra)

;; lab2

#|
Definition = (define-function (Variable Variable1 ...)  Expression)
 	 	 	 	 
Expression = (function-application Variable Expression ...)
          |  (if Expression then Expression else Expression)
          |  (+ Expression Expression)
          |  Variable
          |  Number
          |  String
|#
;; literal as #%datum
(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . n:number) #''n]
    [(_ . s:string) #''s]
    [(_ . b:boolean) #''b]
    [(_ . other) (raise-syntax-error 'literal "Unexpected data." #'other)]))

;; SYNTAX
;; (define-function (f x ...) e)
;; binds f to a syntax tranformer of shape (cons n s)
;; where n is the arity |x ...| of f
;; and   s is syntax for (λ (x ...) e) 
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (f:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     (define f (λ (parameter ...) #'body))
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

;; SYNTAX
;; (function-app f e1 ... eN)
;; applies f to the values of e1 ... IF f is defined and f's arity is N 
(define-syntax (app stx)
  (syntax-parse stx
    [(_ f:id arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (define-values (arity the-function) (lookup #'f stx))
     (cond
       [(= arity n-args)  #`(f arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [_ (raise-syntax-error #f "foobar")]))
  
; Identifier Syntax -> (values N Id)
; EFFECT raises an exception if id is not available
(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

;; define-as-next

(define-for-syntax next 0)

(define-syntax (define-as-next stx)
  (syntax-parse stx
    ((_ var:id) (set! next (+ next 1))
                #`(define var #,(sub1 next)))))

;; if then else

(define-syntax (iff stx)
  (syntax-parse stx #:literals (then else)
    [(_ bool:expr then expt:expr else expf:expr) (syntax/loc stx (if bool expt expf))]))

(define-syntax (then stx)
  (syntax-parse stx
    [(_ a) (raise-syntax-error 'then "keyword `then` is only allowed in if expressions." #'a)]))

(define-syntax (else stx)
  (syntax-parse stx
    [(_ a) (raise-syntax-error 'else "keyword `else` is only allowed in if expressions." #'a)]))

;; =================================

(define-syntax (gen-binop stx)
  (syntax-parse stx
    ((_ (name op) ...) #'(begin
                           (define-syntax (name stx)
                             (syntax-parse stx
                               ((_ m:expr n:expr) #'(op m n))))
                           ...))))
(gen-binop (plus +) (minus -) (mult *) (div /) (gt >)
           (lt <) (biggest max) (smallest min) (eq =)
           (string+ string-append) (both and) (any or)
           (str-eq string=?))


(define-syntax (gen-unary stx)
  (syntax-parse stx
    ((_ (name op) ...) #'(begin
                           (define-syntax (name stx)
                             (syntax-parse stx
                               ((_ m:expr) #'(op m))))
                           ...))))

(gen-unary (iszero zero?) (++ add1) (-- sub1) (str-len string-length))
