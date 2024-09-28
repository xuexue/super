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

(define (member-atom a a*)
  (and (pair? a*)
       (if (atom=? (car a*) a)
           a*
           (member-atom a (cdr a*)))))

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
    [(and (atom=? (car expr) 'letrec) (atom=? (cadr expr) '())) ; base case for letrec
     (eval (caddr expr) env)] ; todo check arity
    [(atom=? (car expr) 'letrec)
     (let ((mapping-rest (cddr expr)) ; todo: arity check on expr
           (sym          (car (car expr)))
           (lam          (cadr (car expr))) ; todo: arity check on (car expr)
           (body         (caddr expr)))
       ; todo: check lam is actually a lambda
       (letrec ((env-new (env-extend* env (list sym (eval lam env-new)))))
         (eval (list 'letrec mapping-rest body) env-new))
       )]
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

  #;(test-equal? "eval of a weird letrec"
               (eval '(letrec ((f (lambda (x) f))) (call f (quote 1))) env.empty)
               '())
  (test-equal? "eval of a not weird letrec"
               (eval '(letrec ((has0? (lambda (lst) 
                                          (if (pair? lst)
                                              (if (atom=? (car lst) (quote 0)) (quote #t) (call has0? cdr lst))
                                              (quote #f)))))
                         (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #t)
)



