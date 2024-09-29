#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar 'x))

(define (drive st)
  (let* ((frames (state-frame* st))
         (cx     (state-constraint st))
         (top    (car frames))
         (rest   (cdr frames))
         (op     (frame-op top))
         (vals   (frame-vals top))
         (env    (frame-env top)))
    (cond
      ((symbol? op)
       (case op
         ((halt) (list st))
         ((call) (error "todo"))
         (else
          (cond
            ((assq op (map2 cons '(cons = symbol=? + vector-ref) (list cons = symbol=? + vector-ref)))
             => (lambda (name&proc) (error "todo")))
            ((assq op (map2 cons '(car cdr) (list car cdr)))
             => (lambda (name&proc)
                  (let ((val  (car vals))
                        (proc (cdr name&proc)))
                    (cond [(lvar? val)
                           (list
                             (let* ((lv1 (new-var)) ; names of the fresh logic variable
                                    (lv2 (new-var))
                                    (c^  (cx:and cx (list '= (cons lv1 lv2) val))))
                                (state (frames-pushval rest lv1) c^))
                             (let* ((c^  (cx:and cx (list 'not (list 'has-type 'pair val)))))
                                (state (frames-error frames) c^)))]
                          [(pair? val) (list (state (frames-pushval rest (proc val)) cx))]
                          [else        (list (state (frames-error frames) cx))]))))
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
    (list (state (step car-frames) constraint.empty))))
