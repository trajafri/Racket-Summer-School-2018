#lang algebra

(require rackunit)

(check-equal? (plus 1 2) 3)
(check-equal? (plus (plus 1 0) (plus 0 2)) 3)
(check-equal? (minus 1 2) -1)
(check-equal? (minus (minus 1 0) (minus 2 0)) -1)
(check-equal? (minus 9 (minus 2 0)) 7)
(check-equal? (string+ "9" (string+ " 0" " 2")) "9 0 2")
(check-equal? (string+ "cat" (string+ " crow" (string+ " dog" " cow")))
              "cat crow dog cow")
(check-equal? (str-eq "a" "b") #f)
(check-equal? (str-eq "a" "a") #t)

(check-equal? (iszero 0) #t)
(check-equal? (iszero 1) #f)
(check-equal? (++ 0) 1)
(check-equal? (++ 1) 2)
(check-equal? (-- 1) 0)
(check-equal? (-- 2) 1)
(check-equal? (++ 0) 1)
(check-equal? (str-len "a") 1)
(check-equal? (str-len "") 0)
(check-equal? (str-len "abc") 3)

(define (aa b) b)

(define (factorial n)
  (if (iszero n) then 1 else (mult n (app factorial (minus n 1)))))


;(app factorial 1)