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

(define env.empty '())
(define (env-extend env k v) (cons (cons k v) env))


(define (lookup env key)
  (let ((kv (assq key env)))
    (if kv (cdr kv) (error "unbound variable" key))))

(define (eval expr env)
  (define (expr-arity=?! n)
    (let ((arity (length (cdr expr))))
      (unless (atom=? arity n)
        (error "invalid expression arity" expr 'expected n 'actual arity))))
  (cond
    [(symbol? expr)
     (lookup env expr)]
    [(not (pair? expr))
     (error "invalid expression")]
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
                  [(atom=? op 'procedure?) (procedure? v)]
                  )
            (error "to many arguments argument in" op)))]
    [(atom=? (car expr) 'if)
     (if (eval (cadr expr) env)
         (eval (caddr expr) env)
         (eval (cadddr expr) env))]
    [else
     expr]))


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
  (test-equal? "eval comparison of numbers"
               (eval '(atom=? (quote 0) (quote 1)) env.empty)
               #f)
  (test-equal? "eval if expression"
               (eval '(if (quote #t) (quote 0) (quote 1)) env.empty)
               0)



)



