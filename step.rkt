#lang racket

(provide step)
(require "common.rkt")

(module+ test
  (require rackunit))

(struct frame (op vals exprs env) #:prefab)
(define frame.halt
  (frame 'halt '() '() env.empty))
(define (frame-addval f val)
    (frame (frame-op f) (cons val (frame-vals f)) (frame-op f) (frame-env f)))


; takes stack of frames => returns stack of frames
(define (step frames)
  (let* ((top         (car frames))
         (op          (frame-op top))
         (env         (frame-env top))
         (vals        (frame-vals top))
         (exprs       (frame-exprs top))
         (next        (and (pair? (cdr frames)) (cadr frames)))
         (rest-frames (and next (cddr frames))))
    (cond
      ((symbol? op)
       (case op
         ((halt) frames)
         ((call)
          (if (null? exprs)
              (error "TODO: actually call the function")
              (expr->frames (car exprs) ; TODO --- not actually correct
                            env 
                            (cons (frame 'call (frame-vals top) (cdr exprs) env) (cdr frames)))))
         (else
          (cond
            ((assq op (map2 cons '(cons atom=?) (list cons atom=?)))
             => (lambda (op-pair)
                  (let ((proc (cdr op-pair)))
                    (error "TODO"))))
            ((assq op
                   (map2 cons
                         '(car cdr null? boolean? pair? number? symbol? procedure?)
                         (list car cdr null? boolean? pair? number? symbol? closure?)))
             => (lambda (op-pair)
                  (let ((proc (cdr op-pair)))
                    (error "TODO"))))
            (else (error "invalid frame op" top))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (cons (frame-addval next (env-ref env (cadr op))) rest-frames))
         ((quote)  (cons (frame-addval next (cadr op)) rest-frames))
         ((if)     (if (car vals)
                       (expr->frames (cadr op) env (cdr frames))
                       (expr->frames (caddr op) env (cdr frames))))
         ((lambda) (cons (frame-addval next (make-closure op env)) rest-frames))
         ((letrec) (error "TODO"))
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
  (test-equal? "evaluate an if"
               (eval '(if (quote #f) (quote 0) (quote 1)) env.empty)
               1)

)


