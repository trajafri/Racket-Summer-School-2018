#lang racket

;; Types !

;; Why do we want types?
;; Lot of research is done to make our programs predictable without running them.

;; With types, you can prevent bugs, avoid malware, prove equivalence (in case you refactor some code, for example)

;; "All non-trivial, semantic properties of programs are undecidable" - Rice's Theorem

;; So, a type system is a lightweight, syntactic analysis that approximates program behavior

;; Type system in Racket can validate function args (prevent bugs), Rust can check memory safety (avoid malware), and Coq can
;; verify program properties (prove equivalence)

;;===================================================================================================================


;; Road map for a typed lang:

;;  Surface Code ---> Reader/Lexer/Parser --- AST (Syntax objects) ---> Type Checker ---- Checked AST ----> Compiler ---> Core Code/ bytecode
;;                                                                                                  (includes macro expander)

;; How to make a typed lang:

;; 1. Incorporate types into the grammar
;; 2. Come up with a language of types
;; 3. Develop types rules for each lang construct
;; 4. Implement a type checker

;;  ⊢ e : τ   ==   "It is provable (true within your type system, that expression `e` has type `τ`"

(require (for-syntax syntax/parse))

(provide (rename-out [typechecking-mb #%module-begin]))

(begin-for-syntax
  ;; compute : StxExpr -> StxTy
  (define (compute expr)
    (syntax-parse expr
      [_:integer #'Int]
      [_:boolean #'Bool]
      [_:string #'String]
      [els (raise-syntax-error #f "Unexpected expression" expr)])))

(define-syntax (typechecking-mb stx)
  (syntax-parse stx
    [(_ e ...) #:do[(for ([e (syntax->list #'(e ...))])
                      (printf "~a : ~a\n" (syntax->datum e)
                              (syntax->datum (compute e))))]
               #'(#%module-begin (void))]))
