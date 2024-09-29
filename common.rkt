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
(define frame.halt
  (frame 'halt '() '() env.empty))

