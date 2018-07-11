#lang br/quicklang
(require rackunit)

(module+ reader
  (provide read-syntax))

;; dec->binary: Integer [ListOf (List Integer Integer)] -> [ListOf Integer]
;; Converts the given number to a list containing all bits
(define (dec->binary int cache)
  (define (helper int acc)
    (cond
      [(zero? acc) '()]
      [(zero? (modulo int 2)) (cons 0 (helper (/ int 2) (sub1 acc)))]
      [else (cons 1 (helper (floor (/ int 2)) (sub1 acc)))]))
  (define lookup (assoc int cache))
  (if lookup (values (cadr lookup) cache) (let [(bin (helper int 7))]
                                           (values bin (cons `(,int ,bin) cache)))))

(check-equal? (let-values [((bin-num cache) (dec->binary 32 '()))]
                bin-num) '(0 0 0 0 0 1 0))
(check-equal? (let-values [((bin-num cache) (dec->binary 32 '()))]
                cache) `((32 (0 0 0 0 0 1 0))))
(check-equal? (let-values [((bin-num cache) (dec->binary 119 '()))]
                bin-num) '(1 1 1 0 1 1 1))
(check-equal? (let-values [((bin-num cache) (dec->binary 119 '()))]
                cache) '((119 (1 1 1 0 1 1 1))))
(check-equal? (let-values [((bin-num cache) (dec->binary 41 '()))]
                bin-num) '(1 0 0 1 0 1 0))
(check-equal? (let-values [((bin-num cache) (dec->binary 41 '()))]
                cache) '((41 (1 0 0 1 0 1 0))))


;; token->binary: InputStream -> [ListOf [ListOf Numbers]]
;; Converts all characters in the given stream to binary.
(define (token->binary ip cache)
  (define curr-char (read-char ip))
  (if (eof-object? curr-char) '() (let-values [((ans cache) (dec->binary (char->integer curr-char) cache))]
                                          (cons ans (token->binary ip cache)))))

(check-equal? (token->binary (open-input-string "abc") '()) '((1 0 0 0 0 1 1) (0 1 0 0 0 1 1) (1 1 0 0 0 1 1)))

;; bin->taco: [ListOf Number] -> [ListOf Symbols]
;; Converts binary to taco
(define (bin->taco bin)
  (map (λ (x) (if (zero? x) '() 'taco)) bin))

(check-equal? (bin->taco '(1 0 0 0 0 1 1)) '(taco () () () () taco taco))


(define (read-syntax src ip)
  (define binary-list (token->binary ip '()))
  (define taco-fied (map bin->taco binary-list))
  (with-syntax
      [((P ...) taco-fied)]
    #'(module whatever racket
        (displayln 'P) ...)))


#|
(token->binary (open-input-string "abc") '())
'((1 0 0 0 0 1 1) (0 1 0 0 0 1 1) (1 1 0 0 0 1 1))
> (token->binary (open-input-string "\n\n\"hello world\"\n(+ 1 (* 2 (- 3)))") '())
'((0 1 0 1 0 0 0)
  ((0 1 0 1 0 0 0))
  (0 1 0 0 0 1 0)
  (0 0 0 1 0 1 1)
  (1 0 1 0 0 1 1)
  (0 0 1 1 0 1 1)
  ((0 0 1 1 0 1 1))
  (1 1 1 1 0 1 1)
  (0 0 0 0 0 1 0)
  (1 1 1 0 1 1 1)
  ((1 1 1 1 0 1 1))
  (0 1 0 0 1 1 1)
  ((0 0 1 1 0 1 1))
  (0 0 1 0 0 1 1)
  ((0 1 0 0 0 1 0))
  ((0 1 0 1 0 0 0))
  (0 0 0 1 0 1 0)
  (1 1 0 1 0 1 0)
  ((0 0 0 0 0 1 0))
  (1 0 0 0 1 1 0)
  ((0 0 0 0 0 1 0))
  ((0 0 0 1 0 1 0))
  (0 1 0 1 0 1 0)
  ((0 0 0 0 0 1 0))
  (0 1 0 0 1 1 0)
  ((0 0 0 0 0 1 0))
  ((0 0 0 1 0 1 0))
  (1 0 1 1 0 1 0)
  ((0 0 0 0 0 1 0))
  (1 1 0 0 1 1 0)
  (1 0 0 1 0 1 0)
  ((1 0 0 1 0 1 0))
  ((1 0 0 1 0 1 0)))
> 

|#