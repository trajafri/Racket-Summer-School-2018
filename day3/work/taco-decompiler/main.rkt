#lang br/quicklang

(require rackunit)

(module+ reader
  (provide read-syntax))

;; taco->decimal: [ListOf Symbols] -> Char
;; Converts the given tacofied notation of binary digit to an ASCII character
(define (taco->char taco)
  (define (taco->integer taco ans acc) 
    (cond
      [(or (empty? taco) (eof-object? taco)) ans]
      [(eqv? 'taco (car taco)) (taco->integer (cdr taco) (+ acc ans) (* 2 acc))]
      [else (taco->integer (cdr taco) ans (* 2 acc))]))
  (integer->char (taco->integer taco 0 1)))

(check-equal? (taco->char '(taco () () taco () taco ())) #\))
(check-equal? (taco->char '(() taco () taco () () ())) #\newline)


(define (tokenize ip)
  (define current-taco (read-line ip))
  (if (eof-object? current-taco)
      '() (cons (taco->char (read (open-input-string current-taco)))
                (tokenize ip))))

(check-equal? (tokenize (open-input-string
                        "(() taco () () () taco ())
(() () () taco () taco taco)
(taco () taco () () taco taco)
(() () taco taco () taco taco)
(() () taco taco () taco taco)
(taco taco taco taco () taco taco)
(() () () () () taco ())
(taco taco taco () taco taco taco)
(taco taco taco taco () taco taco)
(() taco () () taco taco taco)
(() () taco taco () taco taco)
(() () taco () () taco taco)
(() taco () () () taco ())")) (string->list "\"hello world\""))

(define (parse loc)
  (define (take-until-eq ls char)
    (cond
      [(empty? ls) '()]
      [(eqv? (car ls) char) '()]
      [else (cons (car ls) (take-until-eq (cdr ls) char))]))
  (define (drop-until-eq ls char)
    (cond
      [(empty? ls) '()]
      [(eqv? (car ls) char) ls]
      [else (drop-until-eq (cdr ls) char)]))
  (cond
    [(empty? loc) '()]
    [(eqv? (car loc) #\newline) (cons "\n" (parse (cdr loc)))]
    [else (cons (list->string (take-until-eq loc #\newline)) (parse (drop-until-eq loc #\newline)))]))

(check-equal? (parse (string->list "\"hello world\"")) '("\"hello world\""))
(check-equal? (parse (string->list "\n\"hello world\"\n")) `("\n" "\"hello world\"" "\n"))

(define (read-syntax src ip)
  (define loc (tokenize ip))
  (define out-str (parse loc))
  (with-syntax [((P ...) out-str)]
    #'(module detaco racket
      (display 'P) ...)))