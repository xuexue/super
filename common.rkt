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
(define (make-closure lam env) (closure (cadr lam) (caddr lam) env))
(define (param*? x) (and (list? x) (andmap symbol? x)))
(define (lambda? expr)
  (and (list? expr) (= (length expr) 3) (atom=? (car expr) 'lambda)
       (param*? (cadr expr))))

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
