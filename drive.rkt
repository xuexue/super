#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar (gensym 'x)))

;; TODO:
(define (walk x cx) x)

(define (with-pair x cx on-pair on-error)
  (let ((x (walk x cx)))
    (cond
      ((pair? x) (on-pair x cx))
      ((lvar? x) (append (let ((p (cons (new-var) (new-var))))
                           (on-pair p (cx:and cx (list '= p x))))
                         (on-error (cx:and cx (list 'not (list 'has-type 'pair x))))))
      (else      (on-error cx)))))

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
         ((call) (let* ((vals (reverse vals))
                        (proc (walk (car vals) cx))
                        (arg* (map (lambda (v) (walk v cx)) (cdr vals))))
                   (cond ((lvar? proc)
                          (list (state (frames-stop frames) cx))) ; fix later
                         ((procedure? proc)
                          (let* ((cenv (env-extend* (closure-env proc)
                                                    (closure-param* proc)
                                                    arg*)))
                              (list (state (expr->frames (closure-body proc)
                                                         cenv
                                                         rest) cx))))
                         (else (list (state (frames-error frames) cx))))))
         (else
          (cond
            ((assq op (map2 cons '(cons = symbol=? + vector-ref) (list cons = symbol=? + vector-ref)))
             => (lambda (name&proc) (error "todo")))
            ((assq op (map2 cons '(car cdr) (list car cdr)))
             => (lambda (name&proc)
                  (let ((val  (car vals))
                        (proc (cdr name&proc)))
                    (with-pair val cx
                               (lambda (val cx) (list (state (frames-pushval rest (proc val)) cx)))
                               (lambda (cx)     (list (state (frames-error frames) cx)))))))
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
  (define car-frames (list (frame 'car '((1 . 0)) '() env.empty) frame.halt))
  (define cdr-frames (list (frame 'cdr '((1 . 0)) '() env.empty) frame.halt))
  (test-equal?
    "drive a car"
    (drive (state car-frames constraint.empty))
    (list (state (step car-frames) constraint.empty)))
  (test-equal?
    "drive a cdr"
    (drive (state cdr-frames constraint.empty))
    (list (state (step cdr-frames) constraint.empty)))
)
