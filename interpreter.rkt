#lang racket

(module+ test
  (require rackunit))

(define (atom?  x)   (or (null? x) (boolean? x) (number? x) (symbol? x)))
(define (atom?! x)   (unless (atom? x) (error "not an atom" x)))
(define (atom=? a b) (atom?! a) (atom?! b) (eqv? a b))

(define env.empty '())
(define (env-extend env k v) (cons (cons k v) env))


(define (lookup env key)
  (let ((kv (assq key env)))
    (if kv (cdr kv) (error "unbound variable" key))))

(define (eval expr env)
  (cond
    [(symbol? expr)
     (lookup env expr)]
    [(not (pair? expr))
     (error "invalid expression")]
    [(and (atom=? (car expr) 'quote) (atom? (car (cdr expr))))
     (if (atom=? (cdr (cdr expr)) '())
         (car (cdr expr))
         'error)] ; todo
    [(atom=? (car expr) 'cons)
     (if (atom=? (cdr (cdr (cdr expr))) '())
         (cons (eval (car expr) env)
               (eval (car (cdr expr)) env))
         'error)]
    [else
     expr]))
     

(module+ test
  (test-equal? "eval atom"
               (eval '(quote ()) env.empty)
               '()))

