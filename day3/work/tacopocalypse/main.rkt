#lang br/quicklang

(require brag/support racket/sequence)

(module+ reader
  (provide read-syntax))


(define lex
  (lexer
   ["#$" empty]
   ["%" 'taco]
   [any-char (lex input-port)]
   ))


(define (tokenize ip)
  (define tacolets (for/list ([tok (in-port lex ip)]) tok))
  (for/list ([tok (in-slice 7 tacolets)]) tok))

(define (parse toks)
  (for/list ([tok (in-list toks)])
    (integer->char (for/sum ([val (in-list tok)]
                             [power (in-naturals)]
                             #:when (eqv? val 'taco))
                     (expt 2 power)))))

(define (read-syntax src ip)
  (define tokens (tokenize ip))
  (define parse-tree (parse tokens))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))

