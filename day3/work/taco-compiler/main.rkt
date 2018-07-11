#lang br/quicklang
(require rackunit)

(module+ reader
  (provide read-syntax))

;; dec->binary: Integer -> [ListOf Integer]
;; Converts the given number to a list containing all bits
(define (dec->binary int)
  (define (helper int acc)
    (cond
      [(zero? acc) '()]
      [(zero? (modulo int 2)) (cons 0 (helper (/ int 2) (sub1 acc)))]
      [else (cons 1 (helper (floor (/ int 2)) (sub1 acc)))]))
  (helper int 7))

(check-equal? (dec->binary 32) '(0 0 0 0 0 1 0))
(check-equal? (dec->binary 119) '(1 1 1 0 1 1 1))
(check-equal? (dec->binary 41) '(1 0 0 1 0 1 0))


;; token->binary: InputStream -> [ListOf [ListOf Numbers]]
;; Converts all characters in the given stream to binary.
(define (token->binary ip)
  (define curr-char (read-char ip))
  (if (eof-object? curr-char) '() (cons (dec->binary (char->integer curr-char)) (token->binary ip))))

(check-equal? (token->binary (open-input-string "abc")) '((1 0 0 0 0 1 1) (0 1 0 0 0 1 1) (1 1 0 0 0 1 1)))

;; bin->taco: [ListOf Number] -> [ListOf Symbols]
;; Converts binary to taco
(define (bin->taco bin)
  (map (λ (x) (if (zero? x) '() 'taco)) bin))

(check-equal? (bin->taco '(1 0 0 0 0 1 1)) '(taco () () () () taco taco))


(define (read-syntax src ip)
  (define binary-list (token->binary ip))
  (define taco-fied (map bin->taco binary-list))
  (with-syntax
      [((P ...) taco-fied)]
    #'(module whatever racket
        'P ...)))