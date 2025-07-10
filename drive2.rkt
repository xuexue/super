#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)
(require racket/pretty)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar (gensym 'x)))

; drive
(define (drive st (rho '())) ; st: STATE, rho: TBD -> dnode
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
         ((halt) (dnode:done st))
         ; what should "walk" do?
         ((call) (dnode:todo op))
         ((cons) (dnode:transient (state (frames-pushval rest (cons (walk (cadr vals) cx) (walk (car vals) cx))) cx)))
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
              (lambda (name&proc)
                (let ((val  (car vals))
                      (proc (cdr name&proc)))
                  (cond
                    ((pair? val)
                     (dnode:transient (state (frames-pushval rest (proc val)) cx)))
                    ((lvar? val)
                     (dnode:todo op))
                     #;(case (cx*:check val (cx:has-type 'pair?) cx*)
                          ((#t) (dnode:transient (state (frames-pushval rest (proc val)) cx))
                          ((#f) (dnode:transient (state (frames-error frames) cx)))
                          (else (dnode:if `(pair? ,val) ; expression
                                          (state (frames-pushval rest (proc val)) (cx*:and cx val (cx:has-type 'pair)))
                                          (state (frames-error frames)            (cx*:and cx val (cx:not-type 'pair)))))))

                  ))))
             ((assq op types-to-ops)
              =>
              (dnode:todo op))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
        (case (car op)
          ((lookup) (dnode:transient (state (frames-pushval rest (env-ref env (op:lookup-v op))) cx)))
          ((quote) (dnode:transient (state (frames-pushval rest (quote-a op)) cx)))
          ((if) (dnode:todo op))
          ((lambda) (dnode:todo op))
          ((letrec) (dnode:todo op))
          (else (error "invalid frame op" top)))))))


