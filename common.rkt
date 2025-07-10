#lang racket/base
(provide (all-defined-out))
(require racket/bool)
(require racket/match)

(define (atom? x) (or (null? x) (boolean? x) (number? x) (symbol? x)))
(define (atom=? a b)
  (cond ((null?    a) (null? b))
        ((boolean? a) (and (boolean? b) (if a b (not b))))
        ((number?  a) (and (number? b) (= a b)))
        ((symbol?  b) (and (symbol? b) (symbol=? a b)))
        (else         #f)))

(define (not x) (if x #f #t))
(define (caar  x) (car (car x)))
(define (cadr  x) (car (cdr x)))
(define (cdar  x) (cdr (car x)))
(define (cddr  x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

(define (equal? a b)
  (cond
    ((pair? a) (and (pair? b)
                    (equal? (car a) (car b))
                    (equal? (cdr a) (cdr b))))
    (else (atom=? a b))))

(define (length x*)
  (if (null? x*)
      0
      (+ (length (cdr x*)) 1)))

(define (member a a*)
  (and (pair? a*)
       (if (equal? (car a*) a)
           a*
           (member a (cdr a*)))))

(define (append x* y)
  (if (null? x*)
      y
      (cons (car x*) (append (cdr x*) y))))

(define (map f x*)
  (if (null? x*)
      '()
      (cons (f (car x*)) (map f (cdr x*)))))

(define (map2 f x* y*)
  (if (null? x*)
      '()
      (cons (f (car x*) (car y*)) (map2 f (cdr x*) (cdr y*)))))

(struct closure (param* body env) #:prefab)
(define (make-closure lam env) (closure (lambda-param* lam) (lambda-body lam) env))

(define env.empty '())
(define (env-extend*     env k* v*)   (list 'call   (map2 cons k* v*)   env))
(define (env-extend*/rec env k* lam*) (list 'letrec (map2 cons k* lam*) env))

(define (env-ref env key)
  (when (null? env) (error "unbound variable" key))
  (case (car env)
    ((call)   (let ((kv (assq key (cadr env))))
                (if kv (cdr kv) (env-ref (caddr env) key))))
    ((letrec) (let ((kl (assq key (cadr env))))
                (if kl (make-closure (cdr kl) env) (env-ref (caddr env) key))))
    (else     (error "invalid environment tag" env))))

(define (env-walk env cx)
  (if (equal? env env.empty)
    env.empty
    (let ((envtype (car env))
          (kv*     (cadr env))
          (env^    (caddr env)))
      (define (kv-walk kv)
        (let ((k (car kv)) (v (cdr kv)))
          (cons k (walk v cx))))
      (case envtype
        ((call)   (list 'call (map kv-walk kv*) (env-walk env^ cx)))
        ((letrec) (list 'letrec kv* (env-walk env^ cx)))
        (else     (error "invalid environment tag" env))))))

(define (quote-a E) (cadr E))

(define (if-c E) (cadr   E))
(define (if-t E) (caddr  E))
(define (if-f E) (cadddr E))

(define (call-rator E) (cadr E))
(define (call-rand* E) (cddr E))

(define (param*? x) (and (list? x) (andmap symbol? x)))
(define (lambda? x)
  (and (list? x) (= (length x) 3) (atom=? (car x) 'lambda)
       (param*? (cadr x))))
(define (lambda-param* E) (cadr  E))
(define (lambda-body   E) (caddr E))

(define (binding? x) (and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))))
(define (binding*? x*) (andmap binding*? x*))
(define (binding-lhs b) (car  b))
(define (binding-rhs b) (cadr b))
(define (letrec-binding* E) (cadr E))
(define (letrec-body     E) (caddr E))

(define (primop-rand1 E) (cadr E))
(define (primop-rand2 E) (caddr E))

(define (op:lookup-v op) (cadr op))
(define (op:if-t op)     (cadr op))
(define (op:if-f op)     (caddr op))

(struct frame (op vals exprs env) #:prefab)
(define frame.halt  (frame 'halt  '() '() env.empty))
(define frame.error (frame 'error '() '() env.empty))
(define frame.stop  (frame 'stop  '() '() env.empty))
(define (frames-error frames) (cons frame.error frames))
(define (frames-stop  frames) (cons frame.stop  frames))


; logic variable
(struct lvar (name) #:prefab)

; type-mapping
(define types-to-ops
   (map2 cons
         '(null? boolean? pair? number? symbol? procedure? vector?)
         (list null? boolean? pair? number? symbol? procedure? vector?)))

; constraint constructors (TODO: represent top and bottom?)
(struct cx (op payload) #:prefab)
(define (cx:has-type type) (cx 'has-type type))
(define (cx:not-type type) (cx 'not-type type))
(define (cx:= val)         (cx '= val))
(define (cx:not-= val)     (cx 'not-= val))


(define lvar=>cx.empty '()) ; { lvar: [ cx ... ] }

(define (lvar=>cx-add cx* lvar cx)
  (let ((entry (assq lvar cx*)))
    (if entry
        (cdr entry)
        '())))

(define (value-of-type? v t)
  (case t
    [(null?)     (null? v)]
    [(boolean?)  (boolean? v)]
    [(pair?)     (pair? v)]
    [(number?)   (number? v)]
    [(symbol?)   (symbol? v)]
    [(procedure?)(procedure? v)]
    [(vector?)   (vector? v)]
    [else        #f]))

(define (types-compatible? t1 t2)
  (eq? t1 t2))
      
(define (conflict? cx existing)
  (let ((op1 (cx-op cx))
        (pl1 (cx-payload cx))
        (op2 (cx-op existing))
        (pl2 (cx-payload existing)))
    (match (list op1 op2)
      [(list 'has-type 'not-type)
       (equal? pl1 pl2)]
      [(list 'not-type 'has-type)
       (equal? pl1 pl2)]
      [(list '= 'not-=)
       (equal? pl1 pl2)]
      [(list 'not-= '=)
       (equal? pl1 pl2)]
      [(list 'has-type '=)
       (not (value-of-type? pl2 pl1))]
      [(list '= 'has-type)
       (not (value-of-type? pl1 pl2))]
      [(list 'has-type 'has-type)
       (not (types-compatible? pl1 pl2))]
      [(list '= '=)
       (not (equal? pl1 pl2))]
      [_ #f])))



(define (cx-conflicts? cx-list cx)

  (foldl 
     (lambda (e cx-list^)
        (cond
         [(conflict? cx e) #f]
         [(subsumes? cx e) cx-list^] ; new cx subsumes e -- do not add acc
         [(subsumes? e cx) ] ; old e subsumes new cx; would like to short-circuit so, so maybe not fold?
      cx-list
  ))


;; 5 possible constraint states for an lvar (forms a lattice):
;; - no constraints (top of lattice)
;; - any number of not-type and not-= constraints (the not-type constraints may obviate some not-= constraints)
;; - single type constraint and possible not-= constraints (the type constraint may obviate some not-= constraints)
;; - single boolean type constraint (no not-= constraints possible, since boolean exclusions would be simplified to equality)
;; - single equality constraint
;; So we can implement constraint conflict detection and subsumption via lattice-meet, which will also allow us to merge two logic variables later

;; New lattice-based constraint representation
(struct cx-top () #:prefab) ; top element - no constraints
(struct cx-not (not-types not-vals) #:prefab) ; not-types: set of types to exclude, not-vals: set of values to exclude
(struct cx-type (type not-vals) #:prefab) ; type: the required type, not-vals: set of values to exclude
(struct cx-boolean () #:prefab) ; boolean type constraint (no not-vals possible)
(struct cx-eq (val) #:prefab) ; val: the required value

;; Lattice meet operation - returns the greatest lower bound or #f if inconsistent
(define (cx-meet cx1 cx2)
  (match* (cx1 cx2)
    ;; Top element cases
    [((cx-top) cx2) cx2]
    [(cx1 (cx-top)) cx1]
    
    ;; Equality cases
    [((cx-eq val1) (cx-eq val2))
     (and (equal? val1 val2) (cx-eq val1))]
    [((cx-eq val) (cx-type type not-vals))
     (and (value-of-type? val type) (not (set-member? not-vals val)) (cx-eq val))]
    [((cx-type type not-vals) (cx-eq val))
     (cx-meet (cx-eq val) (cx-type type not-vals))]
    [((cx-eq val) (cx-boolean))
     (and (boolean? val) (cx-eq val))]
    [((cx-boolean) (cx-eq val))
     (cx-meet (cx-eq val) (cx-boolean))]
    [((cx-eq val) (cx-not not-types not-vals))
     (and (not (set-member? not-types (type-of val))) (not (set-member? not-vals val)) (cx-eq val))]
    [((cx-not not-types not-vals) (cx-eq val))
     (cx-meet (cx-eq val) (cx-not not-types not-vals))]
    
    ;; Boolean cases
    [((cx-boolean) (cx-boolean))
     (cx-boolean)]
    [((cx-boolean) (cx-not not-types other-not-vals))
     (and (not (set-member? not-types 'boolean?)) 
          (let ((all-not-vals other-not-vals))
            (cond
              [(set-member? all-not-vals #t) (cx-eq #f)]
              [(set-member? all-not-vals #f) (cx-eq #t)]
              [else (cx-boolean)])))]
    [((cx-not not-types other-not-vals) (cx-boolean))
     (cx-meet (cx-boolean) (cx-not not-types other-not-vals))]
    
    ;; Type cases
    [((cx-type type1 not-vals1) (cx-type type2 not-vals2))
     (and (types-compatible? type1 type2) (cx-type type1 (set-union not-vals1 not-vals2)))]
    [((cx-type type not-vals) (cx-not not-types other-not-vals))
     (and (not (set-member? not-types type)) 
          (let ((filtered-not-vals (set-filter (lambda (v) (not (set-member? not-types (type-of v)))) not-vals))
            (cx-type type (set-union filtered-not-vals (set-filter (lambda (v) (value-of-type? v type)) other-not-vals)))))]
    [((cx-not not-types other-not-vals) (cx-type type not-vals))
     (cx-meet (cx-type type not-vals) (cx-not not-types other-not-vals))]
    
    ;; Not cases
    [((cx-not not-types1 not-vals1) (cx-not not-types2 not-vals2))
     (cx-not (set-union not-types1 not-types2) (set-union not-vals1 not-vals2))]))

;; Helper function to get the type of a value
(define (type-of val)
  (cond
    [(null? val) 'null?]
    [(boolean? val) 'boolean?]
    [(pair? val) 'pair?]
    [(number? val) 'number?]
    [(symbol? val) 'symbol?]
    [(procedure? val) 'procedure?]
    [(vector? val) 'vector?]
    [else 'unknown]))


(define (check-constraint val type cx*)
  (if (lvar? val)
      (lvar=>cx-add cx* val (cx:has-type type))
      'check-if-value-has-the-right-type))

;(define (cx*:and cx* lvar cx) 'TODO)


; driving nodes
(struct dnode (op payload) #:prefab)
(define (dnode:done state)      (dnode 'done state))
(define (dnode:transient state) (dnode 'transient state))
(define (dnode:if e s1 s2)      (dnode 'if `(,e ,s1 ,s2)))
(define (dnode:todo debug)      (dnode 'TODO debug)) ; TODO :remove
(define (dnode:error debug)     (dnode 'error debug)) 
;(define (dnode-if-e n)  (car (dnode-payload n))
;(define (dnode-if-s1 n) (cadr (dnode-payload n))
;(define (dnode-if-s2 n) (caddr (dnode-payload n))

(define (dnode:canfail constraints cx th-frames)
  ; assume constraints has only one (value, type) pair
  (match constraints
    ((list (cons val type))
     (let ((cx^ (check-constraint val type cx)))
       (cond
         [(eqv? cx cx^) ; we can make this less expensive
          (dnode:transient (state (th-frames) cx^))]
         [(not cx^) 
          (dnode:error constraints)]
         [else
          (dnode 'canfail `(,constraints ,(state (th-frames) cx^)))])))))

;; TODO:
(define (walk x cx) x) ; value walk

(define (state frame* constraint) (list frame* constraint))
(define (state-frame*     st) (car  st))
(define (state-constraint st) (cadr st))
(define (state-reify st)
  (let* ((frames (state-frame* st))
         (cx     (state-constraint st)))
    (state (map (lambda (fr)
                  (let* ((vals   (frame-vals fr))
                         (vals^  (map (lambda (v) (walk v cx)) vals))
                         (env    (frame-env fr))
                         (env^   (env-walk env cx)))
                    (frame (frame-op fr) vals^ (frame-exprs fr) env^)))
                frames)
           cx)))
