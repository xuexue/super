#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)
(require racket/pretty)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar (gensym 'x)))

; drive
(define (drive st rho) ; st: STATE, rho: TBD -> dnode
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
         ((halt) (dnode:done))
         ((call) (dnode:todo op))
         ((cons) (dnode:todo op))
         ((vector-ref) (dnode:todo op))
         ((=) (dnode:todo op))
         ((symbol-=?) (dnode:todo op))
         ((vector) (dnode:todo op))
         (else
          (cond
            ((assq op (map2 cons '(+) (list +)))
             =>
             (dnode:todo op))
            ((assq op (map2 cons '(car cdr) (list car cdr)))
             =>
             (dnode:todo op))
            ((assq op
                   (map2 cons
                         '(null? boolean? pair? number? symbol? procedure? vector?)
                         (list null? boolean? pair? number? symbol? procedure? vector?)))
             =>
             (dnode:todo op))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (dnode:todo op))
         ((quote) (dnode:todo op))
         ((if) (dnode:todo op))
         ((lambda) (dnode:todo op))
         ((letrec) (dnode:todo op))
         (else (error "invalid frame op" top)))))))


