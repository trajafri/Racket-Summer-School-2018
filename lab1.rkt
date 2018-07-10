#lang racket

(require (for-syntax syntax/parse racket/list) rackunit)

;; EX 1

(define-syntax (define-world* stx)
  (syntax-parse stx
    [(_ id1:id) #`(define id1 0)]
    [(_ id1:id ...) #'(define-world 0 id1 ...)]))

(define-syntax (define-world stx)
  (syntax-parse stx
    [(_ acc id1:id) #`(define id1 acc)]
    [(_ acc id1:id id2:id ...) #`(begin (define id1 acc)
                                        (define-world (add1 acc) id2 ...))]))

(define-world* a)
(define-world* b c d e)

;; EX 2

(define-syntax (loop stx)
  (syntax-parse stx
    [(_ var0:id ((var1:id exp1:expr) ...) exp0:expr ...)
     #'(let ()
         (define var0
           (λ (var1 ...)
             exp0 ...))
         (var0 exp1 ...))]))

(check-equal? (loop sum ((ls (list 1 1 1))
                         (i 0))
                    (if (empty? ls) i (sum (cdr ls) (add1 i)))) 3)


(check-equal? (loop sum ((ls (list 1 1 1))
                         (i 0)
                         (j 0))
                    (if (empty? ls) (+ i j) (sum (cdr ls) (add1 i) (+ j i)))) 6)

;; EX 3

(define-syntax (all stx)
  (syntax-parse stx
    [(_ e0:expr) #`(let [(ne0 e0)]
                     (and ne0 `(,ne0)))]
    [(_ e0:expr e1:expr ...) #`(let [(ne0 e0)]                 
                                 (and ne0 (let [(rec (all e1 ...))]
                                            (and rec (cons ne0 rec)))))]))

(check-equal? (all #f) #f)

(check-equal? (all #t) '(#t))
(check-equal? (all #t #f) #f)
(check-equal? (all #f #t) #f)
(check-equal? (all #f #f) #f)
(check-equal? (all #t #t #t #t) '(#t #t #t #t))
(check-equal? (all #t #t #t #f) #f)
(check-equal? (all 1 2 3 #f) #f)
(check-equal? (all 1 2 3 4) '(1 2 3 4))

;; EX 4

(define-syntax (dispatch stx)
  (syntax-parse stx #:datum-literals (orelse)
    [(_ exp:expr
        [(x ...) expr0:expr])
     #'(if (member exp '(x ...)) expr0 (error "heck"))]
    [(_ exp:expr
        [(x ...) expr0:expr] [orelse exprn:expr])
     #'(if (member exp '(x ...)) expr0 exprn)]
    [(_ exp:expr
        [(x ...) expr0:expr]
        dc0
        ...)
     #'(if (member exp '(x ...)) (begin expr0 (dispatch exp dc0 ...)) (dispatch exp dc0 ...))]))


(define (f x)
  (dispatch x
            [(x y) (displayln 0)]
            [(z x) (displayln 1)]
            [orelse (displayln 2)]))


;; EX 5

(define-syntax (define-rewrite-rule stx)
  (syntax-parse stx
    [(_ (func expr ...) body) #'(define-syntax (func stx)
                                  (syntax-parse stx
                                    [(_ expr ...) #'body]))]))

(define-rewrite-rule
  (loop-for-ever exp)
  ; —> 
  (local ((define (for-ever) (begin exp (for-ever)))) (for-ever)))

(define-rewrite-rule
  (g a b c d e)
  ; —> 
  (+ a b c d e))


;; EX 6

(define-syntax (where stx)
  (syntax-parse stx
    [(_ body [var0:id expr0:expr] ...) #'(letrec ([var0 expr0] ...) body)]))

(check-equal? (where (op 10 (+ my-favorite-number an-ok-number))
                     [my-favorite-number 8]
                     [an-ok-number 2]
                     [op *]) 100)
(check-equal? (where 100) 100)

(define-syntax (where* stx)
  (syntax-parse stx
    [(_ body [var0:id expr0:expr] ...) #`(where body #,@(reverse (syntax->list #'([var0 expr0] ...))))]))

(check-equal? (where* (list x y z)
                      [x (+ y 4)]
                      [y (+ z 2)]
                      [z 1]) (list 7 3 1))

(check-equal? (where* (z x)
                      [x (+ y 2)]
                      [y 3]
                      [z ((λ (f)
                            (λ (n)
                              (if (zero? n) 1 (* n ((f f) (sub1 n))))))
                          (λ (f)
                            (λ (n)
                              (if (zero? n) 1 (* n ((f f) (sub1 n)))))))]) 120)

;; EX 7

(define-syntax (and/v stx)
  (syntax-parse stx #:datum-literals (=>)
    [(_ exp:expr => var:id body) #'(let [(var exp)]
                                     (if var body #f))]
    [(_ exp:expr body) #'(and exp body)]))

(check-equal? (and/v 1 => x (+ x 1)) 2)
(check-equal? (and/v #f => x (+ x 1)) #f)

;; EX 8

#|
Exercise 8. Modify the definition of split-ct from lecture so that it can also deal
with specifications that do not provide literal constants for start and end.

When a non-literal start or end is provided, the range check should happen at run time.
|#

(begin-for-syntax
  (define-syntax-class byte
    (pattern b:nat #:fail-unless (< (syntax-e #'b) 256) "not a byte")))
 
; SYNTAX
; (split-ct tags start end [name:id step (~optional convert)] ...)
; computes the values of the fields name... by successively extracting
; bytes from tags, beginning at start to maximally end
(define-syntax (split-ct stx)
  (syntax-parse stx
    [(_ tags start:integer end:byte [name step:byte (~optional convert)] ...)
     ; ———————————
     ; the static error checking 
     #:do [(define end-int  (syntax-e #'end))
           (define step-int (sum #'(step ...)))]
     #:fail-unless (< step-int end-int) "index out of range"
     ; ———————————
     #`(let ([i start])
         (let*-values ([(i name) (values (+ i step) (extract tags i (+ i step -1)))]
                       ...)
           (values ((~? convert values) name) ...)))]
    ;------------------------------------------------------------
    [(_ tags [name step:byte (~optional convert)] ...)
     ; ———————————
     ; the static error checking 
     #:do [(define end-int   (syntax-e #'256))
           (define step-int (sum #'(step ...)))]
     #:fail-unless (< step-int end-int) "index out of range"
     ; ———————————
     #`(let ([i 0])
         (let*-values ([(i name) (values (+ i step) (extract tags i (+ i step -1)))]
                       ...)
           (values ((~? convert values) name) ...)))]
    ))
 
; [Listof [Syntax Number]] -> Number
; compute the sum of the numbers hidden in syntax 
(define-for-syntax (sum list-of-syntax-numbers)
  (apply + (map syntax-e (syntax->list list-of-syntax-numbers))))

#|
Definition = (define-function (Variable Variable1 ...)  Expression)
 	 	 	 	 
Expression = (function-application Variable Expression ...)
          |  (if Expression Expression Expression)
          |  (+ Expression Expression)
          |  Variable
          |  Number
          |  String
|#

; ;; SYNTAX
; ;; (define-function (f x ...) e)
; ;; binds f to a syntax tranformer of shape (cons n s)
; ;; where n is the arity |x ...| of f
; ;; and   s is syntax for (λ (x ...) e)
 
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (f:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

; ;; SYNTAX
; ;; (function-app f e1 ... eN)
; ;; applies f to the values of e1 ... IF f is defined and f's arity is N 
 
(define-syntax (function-app stx)
  (syntax-parse stx #:datum-literals (plus minus string+)
    #;[(_ plus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(+ arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    #;[(_ minus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(- arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    #;[(_ string+ arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(string-append arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    #;[(_ ++ arg:expr ...)
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
       [(= arity n-args) #`(#,the-function arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]))

;; EX 9

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

(check-equal? (function-app plus 1 2) 3)
(check-equal? (function-app plus (function-app plus 1 0) (function-app plus 0 2)) 3)

(check-equal? (function-app minus 1 2) -1)
(check-equal? (function-app minus (function-app minus 1 0) (function-app minus 2 0)) -1)
(check-equal? (function-app minus 9 (function-app minus 2 0)) 7)

;; EX 10

(check-equal? (function-app string+ "9" (function-app string+ " 0" " 2")) "9 0 2")
(check-equal? (function-app string+ "cat" (function-app string+ " crow" (function-app string+ " dog" " cow")))
              "cat crow dog cow")

(check-equal? (function-app ++ 0) 1)
(check-equal? (function-app ++ (function-app ++ 2)) 4)
(check-equal? (function-app ++ (function-app plus (function-app ++ 3) (function-app minus 9 7))) 7)

;; EX 11
(define-for-syntax next 0)

(define-syntax (define-as-next stx)
  (syntax-parse stx
    ((_ var:id) (set! next (+ next 1))
                #`(define var #,(sub1 next)))))

;(define-as-next x)
;(define-as-next y)
;(define-as-next z)

(define-as-next x)  ; first define-as-next, so x = 0
(define get-y        
  (λ ()
    (define-as-next y)
    y))  ;; function that returns y's value (where y = 2 because this is a runtime function. z is bounded to 1 before this)
(define 1y (get-y))            ;; 1y = 2

(define another-y (get-y))     ;; another-y = 2
(define-as-next z)             ;; z = 1

(module server racket
  (require (for-syntax syntax/parse))
  (provide define-as-next)
  (define-for-syntax next 0)
  (define-syntax (define-as-next stx)
    (syntax-parse stx
      ((_ var:id) (set! next (+ next 1))
                  #`(define var #,(sub1 next))))))
 
(module client racket
  (require (submod ".." server))
  (define-as-next x)
  (define-as-next y)
  (define-as-next z) x y z)

(module client2 racket
  (require (submod ".." server))
  (define-as-next x)
  (define-as-next y)
  (define-as-next z) x y z)

(require 'client)
(require 'client2)
