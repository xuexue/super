#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)

(module+ test
  (require rackunit))

(define (drive st)
  (let* ((frames     (state-frame* st))
         (constraint (state-constraint st))
         (top        (car frames))
         (op         (frame-op top))
         (vals       (frame-vals top))
         (env        (frame-env top)))
    (cond
      ((symbol? op)
       (case op
         ((halt) (list st))
         ((call) (error "todo"))
         (else
          (cond
            ((assq op (map2 cons '(cons = symbol=? + vector-ref) (list cons = symbol=? + vector-ref)))
             => (lambda (name&proc) (error "todo")))
            ((assq op
                   (map2 cons '(car cdr)
                         (list car cdr)))
             => (lambda (name&proc) (error "todo")))
            ((assq op
                   (map2 cons
                         '(null? boolean? pair? number? symbol? procedure? vector vector?)
                         (list null? boolean? pair? number? symbol? closure? vector vector?)))
             => (lambda (name&proc) (error "todo")))
            (else (error "invalid frame op" top))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (error "todo"))
         ((quote)  (error "todo"))
         ((if)     (error "todo"))
         ((lambda) (error "todo"))
         ((letrec) (error "todo"))
         (else (error "invalid frame op" top)))))))


(module+ test
  (define car-frames
          (list (frame 'car '((1 . 0)) '() env.empty)
                 frame.halt))
  (test-equal?
    "drive a car"
    (drive (state car-frames constraint.empty))
    (list (step car-frames))))
