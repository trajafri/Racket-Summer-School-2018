#lang racket

(provide #%module-begin
         (rename-out [literals #%datum]
                     [app #%app])
         #%top-interaction)

(require (for-syntax syntax/parse))

(module reader syntax/module-reader
  atomic-taco)

(define-syntax (literals stx)
  (syntax-parse stx
    [(_ . s) #''taco]
    [_ (raise-syntax-error #f "Unexpected literal" stx)]))

(define-syntax (app stx)
  (syntax-parse stx
    [(_ f farg ...) #``(taco ,farg ...)]
    [_ (raise-syntax-error #F "sss" stx)]))