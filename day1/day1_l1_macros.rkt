#lang racket

; for-syntax eg (require (for-syntax racket/list))
; define-syntax: -> %Syntax
; #' : Run this code in run time
; define-styntax : define it in run time.

;; SYNTAX
;; (define-hello world) means identifier 'world' is bound to "good bye"
;(define-syntax (define-hello stx)
;  #`(define #,(cadr (syntax->list stx)) "good bye"))

(require (for-syntax syntax/parse racket/list))

;(define-syntax (define-hello stx)
;  (syntax-parse stx
;    [(_ arg:id) #`(define arg "good bye")])) ; note how we don't need #, to eval arg. syntax-parse does that for us.
;                                             ; Also, :id forces arg to be something that could be an identifier.
;                                             ; With :id, we can't pass 17 to define-hello 
;
;(define-hello world) ; == (define world "good bye")
;world
;
;(define-hello test)  ; == (define test "good bye")
;test
;
;; (define-hello 17)    ; == (define 17 "good bye") which does not make sense

; lets make a macro that defines all given variables to have the same definition.
(define-syntax (define-hello args)
  (syntax-parse args
    [(_ arg:id ...) #'(define-values (arg ...)
                        (values (begin 'arg "good bye") ...))]))

(define-hello a b c d)

(define-syntax (define-hellov2 args)
  (syntax-parse args
    [(_ arg:id ...) #'(begin (define arg "good bye")
                             ...)]))

(define-hellov2 e f g h)


;; SYNTAX
;; (some e0:expr e1:expr ... en:expr)
;; produces the list of all non-#false results of the given expressions
;; until it encounters #false the syntax indicates that there is at least
;; one expression though there might be arbitrarily many
;; if first expression is #false, then (some ...) returns #false
(require rackunit)

(define-syntax (some stx)
  (syntax-parse stx
    [(_ e0:expr) #`(let [(ne0 e0)]
                     (and ne0 `(,ne0)))]
    [(_ e0:expr e1:expr ...) #`(let [(ne0 e0)]                 
                                 (and ne0 (cons ne0 (or (some e1 ...) '()))))]))
                             ;(combine #'e0 #'(some e1 ...))]))

#;(define-for-syntax (combine e0  some-of-e1)
    #`(let [(v #,e0)]
        (if v (let [(w #,some-of-e1)]
                (if (pair? w)
                    (cons v w)
                    (list v)))
            #f)))

(check-equal? (some #f) #f)
(check-equal? (some 3) '(3))
(check-equal? (some #f #f) #f)
(check-equal? (some 1 2 #f 3) '(1 2))
(check-equal? (some 1 2 #f (displayln 'hello?)) '(1 2))
(check-equal? (some #f #f 1 2 3) #f)