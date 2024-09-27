#lang racket

(define (atom?  x)   (or (null? x) (boolean? x) (number? x) (symbol? x)))
(define (atom?! x)   (unless (atom? x) (error "not an atom" x)))
(define (atom=? a b) (atom?! a) (atom?! b) (eqv? a b))

(define env.empty '())
(define (env-extend env k v) (cons (cons k v) env))

(define (lookup env key)
  (let ((kv (assq key env)))
    (if kv (cdr kv) (error "unbound variable" key))))

(define (eval expr env)
  (if (symbol? expr)
      (lookup env expr)
      '()))


