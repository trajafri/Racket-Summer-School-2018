#lang racket

;; Ex 20

;;  Γ ⊢ e1 : Boolean  Γ ⊢ e2 : τ  Γ ⊢ e3 : τ       
;;  -----------------------------------------
;;            Γ ⊢ if e1 e2 e3 : τ


;;    Γ ⊢ e2 : Number   Γ ⊢ e3 : Number       
;;  -------------------------------------
;;        Γ ⊢ e1 + e2 : Number

;; EX 21

;;;;;;;;;;;;;;;;;;;;;;;;
;;   => (synthesis)    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------     ----------------     --------------------
;;  e:Nat => Nat       e:Bool => Bool       e:String => String

;;     Γ ,x = τ        Γ ⊢ rator => (-> τ1 τ)   Γ ⊢ rand <= τ1
;;  --------------   -------------------------------------------
;;    Γ ⊢ x => τ                 Γ ⊢ (rator rand) => τ

;;   Γ ⊢ e1 <= Bool  Γ ⊢ e2 => τ1  Γ ⊢ e3 <= τ1              Γ ⊢ e2 <= Number   Γ ⊢ e3 <= Number 
;;  ------------------------------------------           ---------------------------------------
;;          Γ ⊢ if e1 e2 e3 => τ1                                 Γ ⊢ e1 + e2 => Number

;;;;;;;;;;;;;;;;;;;;;;;;
;;    <= (check)      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;      (cons [x:τ] Γ) ⊢ e => t2           Maybe define-function
;;   -------------------------------  
;;     Γ ⊢ λ[x:τ].e => (-> t1 t2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax syntax/parse syntax/stx syntax/id-table))

(provide (rename-out [typechecking-mb #%module-begin])
         if + λ)


; A TyEnv is a [ImmutableFreeIdTableOf IdStx -> TyStx]
; A TyStx is a syntax object representing a type.
; A ExprStx is a syntax object representing an expression.
; A IdStx is an identifier syntax object.

(begin-for-syntax
  ; mk-empty-env : -> TyEnv
  ; Returns a new type environment with no bindings.
  (define (mk-empty-env)
    (make-immutable-free-id-table))
 
  ; add-to-env : TyEnv IdStx TyStx -> TyEnv
  ; Returns a new type environment that extends the given env with the given binding.
  (define (add-to-env env id type)
    (free-id-table-set env id type))
  
  ; lookup-env : TyEnv IdStx -> TyStx or #f
  ; Looks up the given id in the given env and returns corresponding type. Returns false if the id is not in the env.
  (define (lookup-env env id)
    (free-id-table-ref env id (λ () #f)))
  
  ; compute: ExprStx TyEnv -> TyStx
  ; computes the type of the given term
  (define (compute e env)
    (syntax-parse e
      [:integer #'Int]
      [:string #'String]
      [:boolean #'Bool]
      [e:id #:do[(define type (lookup-env env #'e))]
            #:when type
            type]
      [((~literal λ) (x(~datum :)τ) e)
       #:with te (compute #'e (add-to-env env #'x #'τ))
       #'(-> τ te)]
      [((~literal λ) ([x(~datum :)τ] ...) e)
       #:with te (compute #'e (foldl (λ (x τ acc)
                                       (add-to-env env (datum->syntax #f x) (datum->syntax #f τ)))
                                     env
                                     (syntax->list #'(x ...))
                                     (syntax->list #'(τ ...))))
       #'(-> τ ... te)]
      [((~literal if) e1 e2 e3)
       #:with te2 (compute #'e2 env)
       #:when (and (check #'e1 #'Bool env)
                   (check #'e3 #'te2 env))
       #'te2]
      [((~literal +) e1 e2)
       #:when (and (check #'e1 #'Int env) (check #'e2 #'Int env))
       #'Int]
      [(e1 e2)
       #:with (-> i o) (compute #'e1 env)
       #:when (check #'e2 #'i env)
       #'o]
      [e (raise-syntax-error
          'compute
          (format "could not compute type for term: ~a" (syntax->datum #'e)))]))
 
  ; check : ExprStx TyStx TyEnv -> Bool
  ; checks that the given term has the given type
  (define (check e t-expected env)
    (define t (compute e env))
    (or (type=? t t-expected)
        (raise-syntax-error
         'check
         (format "error while checking term ~a: expected ~a; got ~a"
                 (syntax->datum e)
                 (syntax->datum t-expected)
                 (syntax->datum t)))))
 
  ; type=? : TyStx TyStx -> Bool
  ; type equality here is is stx equality
  (define (type=? t1 t2)
    (or (and (identifier? t1) (identifier? t2) (free-identifier=? t1 t2))
        (and (stx-pair? t1) (stx-pair? t2)
             (= (length (syntax->list t1))
                (length (syntax->list t2)))
             (andmap type=? (syntax->list t1) (syntax->list t2))))))

(define-syntax (typechecking-mb stx)
  (syntax-parse stx
    [(_ e ...) #:do[(for/fold [(env (mk-empty-env))]
                              [(e (syntax->list #'(e ...)))]
                      (define type-of-thing (compute e env))
                      (printf "~a : ~a\n" (syntax->datum e)
                              (syntax->datum (compute e env)))
                      env
                      #;(add-to-env env e type-of-thing))]
               #'(#%module-begin (void))]))

