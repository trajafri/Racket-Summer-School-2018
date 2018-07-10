#lang algebra

(define (aa b) b)

(define (factorial n)
  (if (iszero n) then 1 else (mult n (app factorial (minus n 1)))))


(app factorial 1)