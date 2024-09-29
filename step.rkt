#lang racket

(provide step)
(require "common.rkt")

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
         (env   (frame-env f)))
    (if (null? exprs)
        (cons (frame op (cons val vals) exprs env) (cdr frames))
        (expr->frames (car exprs)
                      env
                      (cons (frame op (cons val vals) (cdr exprs) env)
                            (cdr frames))))))

; takes stack of frames => returns stack of frames
(define (step frames)
  (let* ((top         (car frames))
         (op          (frame-op top))
         (env         (frame-env top))
         (vals        (frame-vals top))
         (exprs       (frame-exprs top)))
    (cond
      ((symbol? op)
       (case op
         ((halt) frames)
         ((call)
          (if (null? exprs)
              (let* ((vals (reverse vals))
                     (proc (car vals))
                     (arg* (cdr vals))
                     (cenv (env-extend* (closure-env proc) (closure-param* proc) arg*)))
                  (expr->frames (closure-body proc) cenv (cdr frames)))
              (expr->frames (car exprs)
                            env
                            (cons (frame 'call vals (cdr exprs) env)
                                  (cdr frames)))))
         (else
          (cond
            ((assq op (map2 cons '(cons atom=?) (list cons atom=?)))
             => (lambda (name&proc)
                  (frames-pushval (cdr frames) (apply (cdr name&proc) (reverse vals)))))
            ((assq op
                   (map2 cons
                         '(car cdr null? boolean? pair? number? symbol? procedure?)
                         (list car cdr null? boolean? pair? number? symbol? closure?)))
             => (lambda (name&proc)
                  (frames-pushval (cdr frames) ((cdr name&proc) (car vals)))))
            (else (error "invalid frame op" top))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (frames-pushval (cdr frames) (env-ref env (cadr op))))
         ((quote)  (frames-pushval (cdr frames) (cadr op)))
         ((if)     (if (car vals)
                       (expr->frames (cadr op) env (cdr frames))
                       (expr->frames (caddr op) env (cdr frames))))
         ((lambda) (frames-pushval (cdr frames) (make-closure op env)))
         ((letrec) (let ((bpair* (cadr op)))
                     (expr->frames (caddr op)
                                   (env-extend*/rec env (map car bpair*) (map cdr bpair*))
                                   (cdr frames))))
         (else (error "invalid frame op" top)))))))

(define (expr->frames expr env rest-frames)
  (cond
    [(symbol? expr)     (cons (frame (list 'lookup expr) '() '() env) rest-frames)]
    [(not (pair? expr)) (error "invalid expression")]
    [else
     (case (car expr)
       ((if) (let ((c (cadr expr))
                   (t (caddr expr))
                   (e (cadddr expr)))
               (expr->frames c env (cons (frame (list 'if t e) '() '() env) rest-frames))))
       ((call) (let ((proc  (cadr expr))
                     (rand* (cddr expr)))
                 (expr->frames proc env (cons (frame 'call '() rand* env) rest-frames))))
       ((quote lambda letrec) (cons (frame expr '() '() env) rest-frames))
       (else (cond
               ((member-atom (car expr) '(cons atom=?))
                => (lambda (op*)
                     (let ((op (car op*))
                           (e1 (cadr expr))
                           (e2 (caddr expr)))
                       (expr->frames e1 env (cons (frame op '() (list e2) env) rest-frames)))))
               ((member-atom (car expr)
                             '(null? boolean? pair? number? symbol? procedure? car cdr))
                => (lambda (op*)
                     (let ((op (car op*))
                           (e  (cadr expr)))
                       (expr->frames e env (cons (frame op '() '() env) rest-frames)))))
               (else (error "invalid expression" expr)))))]))


(define (toframes expr)
    (expr->frames expr env.empty (list frame.halt)))

(define (eval expr env)
  (define (step-until-halt frames)
    (if (and (symbol? (frame-op (car frames)))
             (atom=? (frame-op (car frames)) 'halt))
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

)


