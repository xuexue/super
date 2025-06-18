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




(module+ test
  (define (test-equal-singleton? msg frames (constraints cx*.empty))
    ;(pretty-write (drive (state frames constraints)))
    (test-equal?
      msg
      (map state-reify (drive (state frames constraints)))
      (list (state (step frames) constraints))))

  (define (print-and-test-sequence msg expr n (verbose #f))
    (define initframes (toframes expr))
    (let loop ((frames initframes)
               (i      1))
      (when (<= i n)
        (when verbose (pretty-write frames))
        (test-equal-singleton? (string-append msg "(step " (number->string n) ")") frames)
        (loop (step frames) (+ i 1)))))

  (print-and-test-sequence "drive a car" '(car (quote (1 . 0))) 3)
  (print-and-test-sequence "drive a cdr" '(cdr (quote (1 . 0))) 3)
)
