#lang racket
(require (for-syntax syntax/parse)
         rackunit)

(provide 
 #%module-begin
 #%top-interaction
 app
 plus
 minus
 zero?
 string+
 ++
 --
 min
 max
 div
 and
 or
 gt
 lt
 eq
 (rename-out
  [literal #%datum]
  [define-function define]
  [iff if]))

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
;plus
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
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

;; SYNTAX
;; (function-app f e1 ... eN)
;; applies f to the values of e1 ... IF f is defined and f's arity is N 
(define-syntax (app stx)
  (syntax-parse stx #:datum-literals (plus minus mult string+ ++ zero?)
    #;[(_ zero? arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 1 n-args) #`(zero? arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    #;[(_ plus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(+ arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ mult arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(* arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ minus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(- arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ string+ arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(string-append arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ ++ arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 1 n-args) #`(add1 arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ f:id arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (define-values (arity the-function) (lookup #'f stx))
     (cond
       [(= arity n-args)  #`(#,the-function arg ...)]
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
;
;;; Tests for func-app
;(check-equal? (function-app plus 1 2) 3)
;(check-equal? (function-app plus (function-app plus 1 0) (function-app plus 0 2)) 3)
;(check-equal? (function-app minus 1 2) -1)
;(check-equal? (function-app minus (function-app minus 1 0) (function-app minus 2 0)) -1)
;(check-equal? (function-app minus 9 (function-app minus 2 0)) 7)
;(check-equal? (function-app string+ "9" (function-app string+ " 0" " 2")) "9 0 2")
;(check-equal? (function-app string+ "cat" (function-app string+ " crow" (function-app string+ " dog" " cow")))
;              "cat crow dog cow")
;(check-equal? (function-app ++ 0) 1)
;(check-equal? (function-app ++ (function-app ++ 2)) 4)
;(check-equal? (function-app ++ (function-app plus (function-app ++ 3) (function-app minus 9 7))) 7)

;; define-as-next

(define-for-syntax next 0)

(define-syntax (define-as-next stx)
  (syntax-parse stx
    ((_ var:id) (set! next (+ next 1))
                #`(define var #,(sub1 next)))))

;; if then else

(define-syntax (iff stx)
  (syntax-parse stx #:datum-literals (then else)
    [(_ bool:expr then expt:expr else expf:expr) (syntax/loc stx (if bool expt expf))]))

(define-syntax (then stx)
  (syntax-parse stx
    [(_ a) (raise-syntax-error 'then "keyword `then` is only allowed in if expressions." #'a)]))

(define-syntax (else stx)
  (syntax-parse stx
    [(_ a) (raise-syntax-error 'else "keyword `else` is only allowed in if expressions." #'a)]))



;; =================================

;; register the package

;; module-reader only works with s-exps (?)

(module reader syntax/module-reader
  algebra) ;; algebra?

#;
(module reader racket/base
  (provide read-syntax)
  (define (read-syntax src in)
    ;.. consume from in ...
    #'(module a-mod arith
        0)))

(define-function (plus m n)
  (+ m n))

(define-function (minus m n)
  (- m n))

(define-function (zero? n)
  (zero? n))

(define-function (mult m n)
  (* m n))

(define-function (string+ m n)
  (string-append m n))

(define-function (++ n)
  (add1 n))

(define-function (-- n)
  (sub1 n))

(define-function (and m n)
  (and m n))

(define-function (not n)
  (not n))

(define-function (or m n)
  (or m n))

(define-function (min m n)
  (min m n))

(define-function (max m n)
  (max m n))

(define-function (div m n)
  (/ m n))

(define-function (gt m n)
  (> m n))

(define-function (lt m n)
  (< m n))

(define-function (eq m n)
  (= m n))
