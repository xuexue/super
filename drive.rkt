#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)

(module+ test
  (require rackunit))

; logic variable
(struct lvar (name) #:prefab)

; constraints
(define constraint.empty '())
(define constraint-add   cons)

(define (drive state)
  (let* ((frames     (car state))
         (constraint (cdr state))
         (top        (car frames))
         (op         (frame-op top))
         (env        (frame-env top)))
    (list state)))


(module+ test
  (define car-frame 
          (list (frame 'car  '() '((cons (quote 1) (quote 0))) env.empty)
                 frame.halt))
  (test-equal?
    "drive a car"
    (drive (cons car-frame constraint.empty))
    (list (step car-frame))))
        


