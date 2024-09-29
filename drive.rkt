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
         (env        (frame-env top)))
    (list st)))


(module+ test
  (define car-frames
          (list (frame 'car  '() '((cons (quote 1) (quote 0))) env.empty)
                 frame.halt))
  (test-equal?
    "drive a car"
    (drive (state car-frames constraint.empty))
    (list (step car-frames))))
