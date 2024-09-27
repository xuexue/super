#lang racket

(module+ test
  (require rackunit))

(define (atom?  x)   (or (null? x) (boolean? x) (number? x) (symbol? x)))
(define (atom?! x)   (unless (atom? x) (error "not an atom" x)))
(define (atom=? a b) (atom?! a) (atom?! b) (eqv? a b))
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
         (cons (eval (car (cdr expr)) env)
               (eval (car (cdr (cdr expr))) env))
         'error)]
    [else
     expr]))
     

(module+ test
  (test-equal? "eval empty atom"
               (eval '(quote ()) env.empty)
               '())
  (test-equal? "eval cons of two atoms"
               (eval '(cons (quote ()) (quote ())) env.empty)
               '(() . ()))
)



