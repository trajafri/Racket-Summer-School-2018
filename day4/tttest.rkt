#lang turnstile/quicklang

(provide Int Bool String -> (rename-out
                             [typed-app #%app]
                             [typed-datum #%datum]
                             [typed+ +]
                             [typed-if if]))

(define-base-types Int String Bool)
(define-type-constructor -> #:arity > 0)

(define-typerule typed-datum
  [(_ . n:integer) ≫
   -----
   [⊢ (#%datum . n) ⇒ Int]]
  [(_ . n:boolean) ≫
   -----
   [⊢ (#%datum . n) ⇒ Bool]]
  [(_ . n:string) ≫
   -----
   [⊢ (#%datum . n) ⇒ String]])

(define-primop typed+ + : (-> Int Int Int))

#;(define-typerule (typed-app f e ...) ≫
    [⊢ f ≫ f- ⇒ (~-> tin ... tout)]
    #:fail-unless (= (length (syntax->list #'(e ...)))
                     (length (syntax->list #'(tin ...)))) "+ expected 2 arguments."
    [⊢ e ≫ e- ⇐ tin] ...
    -----
    [⊢ (#%app f- e- ...) ⇒ tout])

(define-typerule typed-app
  [(_ f e ...) ≫
               [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
               ;#:cut 
               #:fail-unless (= (length (syntax->list #'(e ...)))
                                (length (syntax->list #'(τin ...)))) "+ expected 2 arguments."
                                                                     [⊢ e ≫ e- ⇐ τin] ...
                                                                     --------------------
                                                                     [⊢ (#%app f- e- ...) ⇒ τout]]
  [(_ f e ...) ⇐ τout ≫
               [⊢ e ≫ e- ⇒ τin] ...
               [⊢ f ≫ f- ⇐ (-> τin ... τout)]
               --------------------
               [⊢ (#%app f- e- ...)]])

(define-typerule typed-if 
  [(_ b case1 case2) ≫
                     [⊢ b ≫ b- ⇐ Bool]
                     [⊢ case1 ≫ c1- ⇒ τ]
                     [⊢ case2 ≫ c2- ⇐ τ]
                     ---------------------
                     [⊢ (if b- c1- c2-) ⇒ τ]])

(define-typerule typed-λ
  [(_ (x (~datum :) τ) e) ≫
                          [⊢ x ≫ x-]
                          [⊢ e ≫ e- ⇒ τout]
                          ---------------------
                          [⊢ (λ (x) e-) ⇒ τ]])

