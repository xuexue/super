#lang racket/base
(provide (all-defined-out))
(require racket/bool)

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

; constraints
(define cx:true #t)
(define cx:false #f)
(define constraint.empty cx:true)
(define (cx:and c1 c2)
  (cond ((equal? c1 cx:true) c2)
        ((equal? c2 cx:true) c1)
        ((equal? c1 cx:false) cx:false)
        ((equal? c2 cx:false) cx:false)
        (else (list 'and c1 c2))))

(define (cx:not c)
  (cond ((equal? c cx:true) cx:false)
        ((equal? c cx:false) cx:true)
        (else (list 'not c)))) ; #TODO: double not elimination; use structs

(define (cx:has-type type v)
  (if (lvar? v)
    (list 'has-type type v)
    (let* ((type&proc (assq type
                            (map2 cons
                                  '(null? boolean? pair? number? symbol? procedure? vector?)
                                  (list null? boolean? pair? number? symbol? procedure? vector?))))
           (proc (cdr type&proc)))
      (if (proc v)
        cx:true
        cx:false))))

(define (cx:= v1 v2)
  (cond ((equal? v1 v2) cx:true)
        ((and (not (lvar? v1)) (not (lvar? v2))) cx:false)
        (else (list '= v1 v2))))

(define (cx:+= v1 v2 v3)
  (cond
    ((and (number? v1) (number? v2)) ; can we use number?
     (cx:= (+ v1 v2) v3))
    ((and (number? v1) (number? v3)) ; can we use number?
     (cx:= (- v3 v1) v2))
    ((and (number? v2) (number? v3)) ; can we use number?
     (cx:= (- v3 v2) v1))
    (else (list '+= v1 v2 v3))))

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

