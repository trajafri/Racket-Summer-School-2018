#lang racket

;; Modules, provide, require

;; (require (only-in racket/list first [second snd])) to just get the functions.
;; in means incoming stuff
;; (require (rename-in racket/list [second snd])) to import everything but rename second

;; to provide functions/definitions, add (provide bind1 ...) any where
;; (provide (rename-out [five my-favorite-number]))
;; out means outgoing stuff


;; (module whatever racket 10) --- whatever is module name, racket is initial import
;; (require 'whatever) to import whatever. Note the quote. That's to not look for whatever in the library register.

;; (require (submod "..")) requires the wrapping module

;; module* allows requiring wrapping module
;; module+ allows wrapping module to require nested module and the other way. This does need initial import if the wrapper module has an import.

;; if a main submodule is found, racket runs it automatically when imported.

;; test module is ran by racotest.

;; (#%module-begin ...). #%module-begin macro decides what to do with all expressions. In racket, (module blah racket 5) == (module blah racket (#%modile-begin 5)).
;(provide (all-from-out racket)) exports everything from racket.


;; syntax/loc: #%Syntax Expr -> Syntax containing the given expr with the given syntax's location.
;; similarly, we have quasisyntax/loc

;; #lang s-exp "language-name.rkt" to use language-name.rkt

;; every literal from a language is wrapped in #%datum

;; s-exp converts parenthesized syntax to syntax objects.

;; in syntax-parse , we can bind a literal to a variable. For example (syntax-parse stx [(_ . (~and 0 z)) #'z] == 0
;;                      [(_ . other) (raise-syntax-error #f "error message" stx #'other)] can be used to raise error when an unexpected pattern is seen and highlight stx to complain.]

;; #%app is for (something ...). So, an application. () is = #%app. (+ ...) = #%app

;; expr class means allow whatever is allowed

;; providing #%top-interaction will allow REPL use.