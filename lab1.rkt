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
     #'(if (member exp x ...) expr0 (error "heck"))]
    [(_ exp:expr
        [(x ...) expr0:expr] [orelse exprn:expr])
     #'(if (member exp x ...) expr0 exprn)]
    [(_ exp:expr
        [(x ...) expr0:expr]
        dc0
        ...)
     #'(if (member exp x ...) (begin expr0 (dispatch exp dc0 ...)) (dispatch exp dc0 ...))]))

(define-syntax (member stx)
  (syntax-parse stx
    [(_ elem s1) #'(if (eqv? elem 's1) #t #f)]
    [(_ elem s1 s2 ...) #'(or (eqv? 's1 elem) (member elem s2 ...))]))

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

(define-syntax (and/v stx)
  (syntax-parse stx #:datum-literals (=>)
    [(_ exp:expr => var:id body) #'(let [(var exp)]
                                     (if var body #f))]))

(and/v 1 => x (+ x 1))
(and/v #f => x (+ x 1))


;; EX 7

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
           (values ((~? convert values) name) ...)))]))
 
; [Listof [Syntax Number]] -> Number
; compute the sum of the numbers hidden in syntax 
(define-for-syntax (sum list-of-syntax-numbers)
  (apply + (map syntax-e (syntax->list list-of-syntax-numbers))))
