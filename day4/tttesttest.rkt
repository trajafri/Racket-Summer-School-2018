#lang racket

(require "tttest.rkt" turnstile/rackunit-typechecking)

(check-type 5 : Int)
(check-type #f : Bool)
(check-type "5" : String)
(check-type + : (-> Int Int Int))
(check-type (+ 1) : Int)
(check-type (+ 1 2) : Int)

(check-type (if #true 1 2) : Int)
;(check-type (if #true "1" "2") : Int)
(check-type (if #true "1" "2") : String)
(check-type (if #true #t #f) : Bool)
;(check-type (if #true 1 "2") : Int)