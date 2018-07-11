#lang passthrough


"hello"
(+ 1 2)

(((λ (f) (λ (x) (if (zero? x) 1 (* x ((f f) (sub1 x))))))
 (λ (f) (λ (x) (if (zero? x) 1 (* x ((f f) (sub1 x))))))) 5)
