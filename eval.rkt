#lang racket/base
(provide eval)
(module+ test
  (require rackunit))
(require "common.rkt" racket/bool)

(define (eval expr env)
  (define (expr-arity=?! n)
    (let ((arity (length (cdr expr))))
      (unless (= arity n)
        (error "invalid expression arity" expr 'expected n 'actual arity))))
  (cond
    ((symbol? expr) (env-ref env expr))
    ((not (pair? expr)) (error "invalid expression" expr))
    (else
     (case (car expr)
       ((quote) (expr-arity=?! 1) (let ((a (quote-a expr)))
                                    (unless (atom? a) (error "not a quoted atom" a))
                                    a))
       ((if) (expr-arity=?! 3) (if (eval (if-c expr) env)
                                   (eval (if-t expr) env)
                                   (eval (if-f expr) env)))
       ((call) (let ((proc (eval (call-rator expr) env))
                     (arg* (map (lambda (rand) (eval rand env)) (call-rand* expr))))
                 (eval (closure-body proc)
                       (env-extend* (closure-env proc) (closure-param* proc) arg*))))
       ((lambda) (unless (lambda? expr) (error "invalid lambda" expr))
                 (make-closure expr env))
       ((letrec) (expr-arity=?! 2)
                 (let ((bpair* (letrec-binding* expr))
                       (body   (letrec-body expr)))
                   (unless (and (list? bpair*)
                                (andmap list? bpair*)
                                (andmap (lambda (bp) (= (length bp) 2)) bpair*))
                     (error "invalid binding pairs" bpair*))
                   (let ((x*   (map binding-lhs bpair*))
                         (lam* (map binding-rhs bpair*)))
                     (unless (param*? x*) (error "invalid letrec parameters" x*))
                     (unless (andmap lambda? lam*) (error "invalid lambdas" lam*))
                     (eval body (env-extend*/rec env x* lam*)))))
       (else (cond
               ((assq (car expr) (map2 cons '(cons = symbol=? + vector-ref) (list cons = symbol=? + vector-ref)))
                => (lambda (op-pair)
                     (expr-arity=?! 2)
                     ((cdr op-pair) (eval (primop-rand1 expr) env) (eval (primop-rand2 expr) env))))
               ((assq (car expr)
                      (map2 cons
                            '(car cdr null? boolean? pair? number? symbol? procedure? vector vector?)
                            (list car cdr null? boolean? pair? number? symbol? closure? vector vector?)))
                => (lambda (op-pair)
                     (expr-arity=?! 1)
                     ((cdr op-pair) (eval (primop-rand1 expr) env))))
               (else (error "invalid expression" expr))))))))

(module+ test
  (test-equal? "eval empty atom"
               (eval '(quote ()) env.empty)
               '())
  (test-equal? "eval cons of two atoms"
               (eval '(cons (quote ()) (quote ())) env.empty)
               '(() . ()))
  (test-equal? "eval car of cons of two atoms"
               (eval '(car (cons (quote 0) (quote 1))) env.empty)
               0)
  (test-equal? "eval cdr of cons of two atoms"
               (eval '(cdr (cons (quote 0) (quote 1))) env.empty)
               1)
  (test-equal? "eval numl? quote 0"
               (eval '(number? (quote 0)) env.empty)
               #t)
  (test-equal? "eval check that 0 is not a procedure"
               (eval '(procedure? (quote 0)) env.empty)
               #f)
  (test-equal? "eval check that a lambda evaluates to a procedure"
               (eval '(procedure? (lambda () (quote 0))) env.empty)
               #t)
  (test-equal? "eval comparison of numbers"
               (eval '(= (quote 0) (quote 1)) env.empty)
               #f)
  (test-equal? "eval if expression"
               (eval '(if (quote #t) (quote 0) (quote 1)) env.empty)
               0)
  (test-equal? "eval calling a zero argument function"
               (eval '(call (lambda () (quote 4))) env.empty)
               4)
  (test-equal? "eval calling a one argument function"
               (eval '(call (lambda (v) v) (quote 1)) env.empty)
               1)
  (test-equal? "eval calling a three argument function"
               (eval '(call (lambda (x y z) y) (quote 1) (quote 2) (quote 3)) env.empty)
               2)
  (test-equal? "eval nested calling of functions"
               (eval '(call (call (lambda (x) (lambda (y) x)) (quote 1)) (quote 2)) env.empty)
               1)
  (test-equal? "eval passing in functions as args"
               (eval '(call (lambda (f x) (call f x)) (lambda (x) x) (quote 1)) env.empty)
               1)
  (test-equal? "eval addition of numbers"
               (eval '(+ (quote 2) (quote 2)) env.empty)
               4)
  (test-equal? "eval addition of numbers in a function"
               (eval '(call (lambda (v) (+ v (quote 1))) (quote 2)) env.empty)
               3)
  (test-equal? "eval vector and vector-ref"
               (eval '(vector-ref (vector (quote 2)) (quote 0)) env.empty)
               2)
  (test-equal? "eval vector?"
               (eval '(vector? (vector (quote 2))) env.empty)
               #t)

  ;(test-equal? "eval of a weird letrec"
  ;             (eval '(letrec ((f (lambda (x) f))) (call f (quote 1))) env.empty)
  ;             '())
  (test-equal? "eval of a not weird letrec"
               (eval '(letrec ((has0? (lambda (lst)
                                        (if (pair? lst)
                                            (if (= (car lst) (quote 0)) (quote #t) (call has0? (cdr lst)))
                                            (quote #f)))))
                        (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #t)
  (test-equal? "eval of a letrec that counts the lenth of a list"
               (eval '(letrec ((len (lambda (lst)
                                        (if (pair? lst)
                                            (+ (quote 1) (call len (cdr lst)))
                                            (quote 0)))))
                        (call len (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               3)

  (test-equal? "eval of a letrec with 2 functions"
               (eval '(letrec ((has0? (lambda (lst)
                                        (if (pair? lst)
                                            (if (= (car lst) (quote 0)) (call not (quote #t)) (call has0? (cdr lst)))
                                            (call not (quote #f)))))
                               (not   (lambda (x)
                                        (if x (quote #f) (quote #t)))))
                        (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #f)
  )
