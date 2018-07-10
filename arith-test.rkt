#lang algebra


(define (factorial n)
  (if (app zero? n) then 1 else (app mult n (app factorial (app minus n 1)))))

(define (aa b) 1)
