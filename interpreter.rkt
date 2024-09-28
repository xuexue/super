#lang racket/base
(provide eval)
(module+ test
  (require rackunit))
(require "common.rkt")

(define (eval expr env)
  (define (expr-arity=?! n)
    (let ((arity (length (cdr expr))))
      (unless (atom=? arity n)
        (error "invalid expression arity" expr 'expected n 'actual arity))))
  (cond
    [(symbol? expr) (env-ref env expr)]
    [(not (pair? expr)) (error "invalid expression")]
    [(and (atom=? (car expr) 'quote) (expr-arity=?! 1) (atom? (cadr expr)))
     (if (atom=? (cddr expr) '())
         (cadr expr)
         (error "invalid quote" expr))]
    [(member-atom (car expr) '(cons atom=?))
     (expr-arity=?! 2)
     (let ((op (car expr))
           (v1 (eval (cadr expr) env))
           (v2 (eval (caddr expr) env)))
        (if (atom=? (cdddr expr) '())
            (cond [(atom=? op 'cons)   (cons v1 v2)]
                  [(atom=? op 'atom=?) (atom=? v1 v2)])
            (error "to many arguments argument in" op)))]
    [(member-atom (car expr) '(null? boolean? pair? number? symbol? procedure? car cdr))
     (expr-arity=?! 1)
     (let ((op (car expr))
           (v  (eval (cadr expr) env)))
        (if (atom=? (cddr expr) '())
            (cond [(atom=? op 'car)        (car v)]
                  [(atom=? op 'cdr)        (cdr v)]
                  [(atom=? op 'null?)      (null? v)]
                  [(atom=? op 'boolean)    (boolean? v)]
                  [(atom=? op 'pair?)      (pair? v)]
                  [(atom=? op 'number?)    (number? v)]
                  [(atom=? op 'symbol?)    (symbol? v)]
                  [(atom=? op 'procedure?) (closure? v)]
                  )
            (error "to many arguments argument in" op)))]
    [(atom=? (car expr) 'if)
     (expr-arity=?! 3)
     (if (eval (cadr expr) env)
         (eval (caddr expr) env)
         (eval (cadddr expr) env))]
    [(lambda? expr) (make-closure expr env)]
    [(atom=? (car expr) 'call)
     (let ((proc (eval (cadr expr) env))
           (arg* (map (lambda (rand) (eval rand env)) (cddr expr))))
       (eval (closure-body proc)
             (env-extend* (closure-env proc) (closure-param* proc) arg*)))]
    [(atom=? (car expr) 'letrec)
     (expr-arity=?! 2)
     (let ((bpair* (cadr expr))
           (body   (caddr expr)))
       (unless (and (list? bpair*)
                    (andmap list? bpair*)
                    (andmap (lambda (bp) (= (length bp) 2)) bpair*))
         (error "invalid binding pairs" bpair*))
       (let ((x*   (map car bpair*))
             (lam* (map cadr bpair*)))
         (unless (param*? x*) (error "invalid letrec parameters" x*))
         (unless (andmap lambda? lam*) (error "invalid lambdas" lam*))
         (eval body (env-extend*/rec env x* lam*)))) ]
    [else (error "invalid expression" expr)]))

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
               (eval '(atom=? (quote 0) (quote 1)) env.empty)
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

  ;(test-equal? "eval of a weird letrec"
  ;             (eval '(letrec ((f (lambda (x) f))) (call f (quote 1))) env.empty)
  ;             '())
  (test-equal? "eval of a not weird letrec"
               (eval '(letrec ((has0? (lambda (lst)
                                        (if (pair? lst)
                                            (if (atom=? (car lst) (quote 0)) (quote #t) (call has0? (cdr lst)))
                                            (quote #f)))))
                        (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #t)
  (test-equal? "eval of a letrec with 2 functions"
               (eval '(letrec ((has0? (lambda (lst)
                                        (if (pair? lst)
                                            (if (atom=? (car lst) (quote 0)) (call not (quote #t)) (call has0? (cdr lst)))
                                            (call not (quote #f)))))
                               (not   (lambda (x)
                                        (if (atom=? x (quote #t)) (quote #f) (quote #t)))))
                        (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #f)
  )
