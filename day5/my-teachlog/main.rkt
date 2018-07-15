#lang racket/base
(require (for-syntax racket/base syntax/parse)
         syntax/parse/define racket/match racket/stream)

(define (search-top r q)
  empty-stream)

(struct theory (rules sols))
(define empty-theory (theory '() #f))

;; Rule := head and the body
;; => (-> (list head body ...) + new names for the variables)

; theory-add: theory? rule? -> msg and a theory?
(define (theory-add thy new-rule)
  (match-define (theory rules sols) thy)
  (values (when sols
            "Theory changed; Dropping pending solutions")
          (theory (cons new-rule rules) #f)))
(define (theory-query thy query)
  (match-define (theory rules _) thy)
  (theory-next (theory rules (search-top rules query))))
(define (theory-next thy)
  (match-define (theory rules sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?) (values "No solutions" thy)]
    [s (values (stream-first s) (theory rules (stream-rest s)))]))

(provide empty-theory)

(define-syntax (relation stx)
  #'42)

(begin-for-syntax
  (define-syntax-class clause
    #:attributes (vars)))

(define-simple-macro (:- thy h:clause b:clause ...)
  (theory-add thy (λ () (with-vars (v ...)
                          (list h b ...)))))

(define-simple-macro (? thy q:clause)
  (theory-query thy q))

(define-simple-macro (next thy)
  (theory-next thy))

(define (teachlog-do!* thy-b tl-stmt)
  (define-values (result next-thy) (tl-stmt))
  (displayln result)
  (set-box! thy-b next-thy))

(define-syntax (teachlog-do! stx)
  (syntax-parse stx
    [(_ thy:id (~or ((~literal relation) . _)
                    ((~literal data) . _))) #'e]
    [(_ thy:id (m . margs)) #'(teachlog-do!* thy (λ () (m (unbox thy) . margs)))]))

(define-simple-macro (teachlog (~optional (~seq #:theory init-thy:expr)) e ...)
  (let ([the-thy (box (~? init-thy empty-theory))])  ;; ~? if init-thy is bounded, use that else empty-theory
    (teachlog-do! the-thy e) ...
    (unbox the-thy)))

;; Test
(module+ test
  (relation parent 2)
  (relation ancestor 2)

  (define targ
    (teachlog
     (:- (parent "maekar" "aegon-5"))
     (:- (parent "aegon-5" "aerys-2"))
     (:- (parent "aerys-2" "viserys"))
     (:- (parent "aerys-2" "daenerys"))
     (:- (parent "daenerys" #,(string-append "dro" "gon")))
     (:- (ancestor X Y)
         (parent X Y))
     (:- (ancestor X Z)
         (parent X Y)
         (ancestor Y Z))))

  (teachlog #:theory targ
            (? (ancestor X "drogon"))
            (next) (next) (next) (next)))
