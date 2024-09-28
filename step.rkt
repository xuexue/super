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
  (if (and (symbol? (frame-op (car frames)))
           (atom=? (frame-op (car frames)) 'halt))
    frames ; return the frames 
    (let ((f    (car frames)) ; top frame
          (next (cadr frames))) ; next frame
      (cond
        [(and (pair? (frame-op f)) (atom=? (car (frame-op f)) 'quote))
         (cons (frame-addval next (cadr (frame-op f))) (cddr frames))]
        [else
          (error "todo")])
      )))


(define (expr->frames expr env rest-frames)
  (define (expr-arity=?! n)
    (let ((arity (length (cdr expr))))
      (unless (atom=? arity n)
        (error "invalid expression arity" expr 'expected n 'actual arity))))
  (cond
    [(symbol? expr)     (error "TODO")]
    [(not (pair? expr)) (error "invalid expression")]
    [(and (atom=? (car expr) 'quote) (expr-arity=?! 1) (atom? (cadr expr)))
     (if (atom=? (cddr expr) '())
         (cons (frame expr '() '() env) rest-frames)
         (error "invalid quote" expr))]
    [(member-atom (car expr) '(cons atom=?))
     (expr-arity=?! 2)
     (let ((op (car expr))
           (e1 (cadr expr))
           (e2 (caddr expr)))
       (expr->frames e1 env (cons (frame op '() (list e2) env) rest-frames)))]
))

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
  (test-equal? "create frame for a quote"
               (toframes '(quote 0))
               (list (frame '(quote 0) '() '() env.empty)
                     frame.halt))
  (test-equal? "evaluate a quote"
               (eval '(quote 0) env.empty)
               0)
  (test-equal? "create frame for a cons"
               (toframes '(cons (quote 0) (quote 1)))
               (list (frame '(quote 0) '() '() env.empty)
                     (frame 'cons '() '((quote 1)) env.empty)
                     frame.halt))
)


