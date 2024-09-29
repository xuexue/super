#lang racket/base
(provide step)
(require "common.rkt" racket/bool)

(module+ test
  (require rackunit))

(struct frame (op vals exprs env) #:prefab)
(define frame.halt
  (frame 'halt '() '() env.empty))
(define (frames-pushval frames val)
  (let* ((f     (car frames))
         (op    (frame-op f))
         (vals  (frame-vals f))
         (exprs (frame-exprs f))
         (env   (frame-env f))
         (rest  (cdr frames)))
    (if (null? exprs)
        (cons (frame op (cons val vals) exprs env) rest)
        (expr->frames (car exprs)
                      env
                      (cons (frame op (cons val vals) (cdr exprs) env) rest)))))

; takes stack of frames => returns stack of frames
(define (step frames)
  (let* ((top  (car frames))
         (op   (frame-op top))
         (env  (frame-env top))
         (vals (frame-vals top))
         (rest (cdr frames)))
    (cond
      ((symbol? op)
       (case op
         ((halt) frames)
         ((call) (let* ((vals (reverse vals))
                        (proc (car vals))
                        (arg* (cdr vals))
                        (cenv (env-extend* (closure-env proc) (closure-param* proc) arg*)))
                   (expr->frames (closure-body proc) cenv rest)))
         (else
          (cond
            ((assq op (map2 cons '(cons = symbol=? + vector-ref) (list cons = symbol=? + vector-ref)))
             => (lambda (name&proc)
                  (frames-pushval rest (apply (cdr name&proc) (reverse vals)))))
            ((assq op
                   (map2 cons
                         '(car cdr null? boolean? pair? number? symbol? procedure? vector vector?)
                         (list car cdr null? boolean? pair? number? symbol? closure? vector vector?)))
             => (lambda (name&proc)
                  (frames-pushval rest ((cdr name&proc) (car vals)))))
            (else (error "invalid frame op" top))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (frames-pushval rest (env-ref env (op:lookup-v op))))
         ((quote)  (frames-pushval rest (quote-a op)))
         ((if)     (if (car vals)
                       (expr->frames (op:if-t op) env rest)
                       (expr->frames (op:if-f op) env rest)))
         ((lambda) (frames-pushval rest (make-closure op env)))
         ((letrec) (let ((bpair* (letrec-binding* op)))
                     (expr->frames (letrec-body op)
                                   (env-extend*/rec env (map binding-lhs bpair*) (map binding-rhs bpair*))
                                   rest)))
         (else (error "invalid frame op" top)))))))

(define (expr->frames expr env rest-frames)
  (cond
    [(symbol? expr)     (cons (frame (list 'lookup expr) '() '() env) rest-frames)]
    [(not (pair? expr)) (error "invalid expression")]
    [else
     (case (car expr)
       ((if) (let ((c (if-c expr))
                   (t (if-t expr))
                   (f (if-f expr)))
               (expr->frames c env (cons (frame (list 'if t f) '() '() env) rest-frames))))
       ((call) (let ((rator (call-rator expr))
                     (rand* (call-rand* expr)))
                 (expr->frames rator env (cons (frame 'call '() rand* env) rest-frames))))
       ((quote lambda letrec) (cons (frame expr '() '() env) rest-frames))
       (else (cond
               ((member (car expr) '(cons = symbol=? + vector-ref))
                => (lambda (op*)
                     (let ((op (car op*))
                           (e1 (primop-rand1 expr))
                           (e2 (primop-rand2 expr)))
                       (expr->frames e1 env (cons (frame op '() (list e2) env) rest-frames)))))
               ((member (car expr)
                        '(null? boolean? pair? number? symbol? procedure? car cdr vector vector?))
                => (lambda (op*)
                     (let ((op (car op*))
                           (e  (primop-rand1 expr)))
                       (expr->frames e env (cons (frame op '() '() env) rest-frames)))))
               (else (error "invalid expression" expr)))))]))

(define (toframes expr)
    (expr->frames expr env.empty (list frame.halt)))

(define (eval expr env)
  (define (step-until-halt frames)
    (if (and (symbol? (frame-op (car frames)))
             (symbol=? (frame-op (car frames)) 'halt))
        (car (frame-vals (car frames)))
        (step-until-halt (step frames))))
  (let ((frames (expr->frames expr env (list frame.halt))))
    (step-until-halt frames)))


(module+ test
  (test-equal? "create frame for a variable lookup"
               (toframes 'v)
               (list (frame '(lookup v) '() '() env.empty)
                     frame.halt))
  (test-equal? "create frame for a quote"
               (toframes '(quote 0))
               (list (frame '(quote 0) '() '() env.empty)
                     frame.halt))
  (test-equal? "create frame for a cons"
               (toframes '(cons (quote 0) (quote 1)))
               (list (frame '(quote 0) '() '() env.empty)
                     (frame 'cons '() '((quote 1)) env.empty)
                     frame.halt))
  (test-equal? "create frame for a +"
               (toframes '(+ (quote 0) (quote 1)))
               (list (frame '(quote 0) '() '() env.empty)
                     (frame '+ '() '((quote 1)) env.empty)
                     frame.halt))

  (test-equal? "create frame for a pair? call"
               (toframes '(pair? (cons (quote 0) (quote 1))))
               (list (frame '(quote 0) '() '() env.empty)
                     (frame 'cons '() '((quote 1)) env.empty)
                     (frame 'pair? '() '() env.empty)
                     frame.halt))
  (test-equal? "create frame for an if"
               (toframes '(if (pair? (cons (quote 0) (quote 1))) (quote 0) (quote 1)))
               (list (frame '(quote 0) '() '() env.empty)
                     (frame 'cons '() '((quote 1)) env.empty)
                     (frame 'pair? '() '() env.empty)
                     (frame '(if (quote 0) (quote 1)) '() '() env.empty)
                     frame.halt))
  (test-equal? "create frame for a lambda"
               (toframes '(lambda (v) (quote 0)))
               (list (frame '(lambda (v) (quote 0)) '() '() env.empty)
                     frame.halt))
  (test-equal? "create frame for a call"
               (toframes '(call (lambda (v) (quote 0)) (quote 2)))
               (list (frame '(lambda (v) (quote 0)) '() '() env.empty)
                     (frame 'call '() '((quote 2)) env.empty)
                     frame.halt))

  (test-equal? "evaluate a quote"
               (eval '(quote 0) env.empty)
               0)
  (test-equal? "evaluate a lambda and function call"
               (eval '(call (lambda (x y) x) (quote 0) (quote 1)) env.empty)
               0)
  (test-equal? "evaluate a lambda and function call"
               (eval '(call (lambda (x y) x) (quote 1) (quote 0)) env.empty)
               1)

  (test-equal? "evaluate a variable"
               (eval 'v (env-extend* env.empty '(v) '(0)))
               0)
  (test-equal? "evaluate an if"
               (eval '(if (quote #f) (quote 0) (quote 1)) env.empty)
               1)


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
  (test-equal? "eval of a letrec with 2 functions"
               (eval '(letrec ((has0? (lambda (lst)
                                        (if (pair? lst)
                                            (if (= (car lst) (quote 0)) (call not (quote #t)) (call has0? (cdr lst)))
                                            (call not (quote #f)))))
                               (not   (lambda (x)
                                        (if x (quote #f) (quote #t)))))
                        (call has0? (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               #f)
  (test-equal? "eval of a letrec that counts the lenth of a list"
               (eval '(letrec ((len (lambda (lst)
                                        (if (pair? lst)
                                            (+ (quote 1) (call len (cdr lst)))
                                            (quote 0)))))
                        (call len (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ())))))) env.empty)
               3)

)


