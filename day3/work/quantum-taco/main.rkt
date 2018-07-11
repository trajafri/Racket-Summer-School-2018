#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (let [(curr-datum (read ip))]
    (if (eof-object? curr-datum) '() (cons curr-datum (tokenize ip)))))

(define (parse tok)
  (map (λ (x) (if (cons? x) (parse x) 'taco)) tok))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (with-syntax ([PT parse-tree])
    #'(module tacofied racket
        'PT)))