#lang br/quicklang
(require brag/support "grammar.rkt" rackunit)
(provide (all-from-out br/quicklang) (all-defined-out))

(module+ reader
  (provide read-syntax))

(define tokenize
  (lexer
   ["#$" lexeme]
   ["%" lexeme]
   [any-char (tokenize input-port)]
   ))

(define (taco) 1)
(define (not-a-taco) 0)
;(define (taco-program . pieces) pieces)

(define (taco-leaf . pieces)
  (define (pieces->integer taco ans acc) 
    (cond
      [(empty? taco) ans]
      [(= 1 (car taco)) (pieces->integer (cdr taco) (+ acc ans) (* 2 acc))]
      [else (pieces->integer (cdr taco) ans (* 2 acc))]))
  (integer->char (pieces->integer pieces 0 1)))

(define (read-syntax src ip)
  (define token-thunk (λ () (tokenize ip)))
  (define parse-tree (parse token-thunk))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module winner taco-victory
         (display (apply string PT))))))

(check-equal?
 (apply-lexer tokenize "\n#$#$#$%#$%%\n")
 '("#$" "#$" "#$" "%" "#$" "%" "%"))


(check-equal?
 `(,(taco-leaf (not-a-taco) (not-a-taco) (not-a-taco) (taco) (not-a-taco) (taco) (taco)))
 '(#\h))