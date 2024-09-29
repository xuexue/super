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
                         (on-error (cx:and cx (list 'not (list 'has-type 'pair? x))))))
      (else      (on-error cx)))))

;; This assumes all vectors contain a single element.
(define (with-vector x cx on-vector on-error)
  (let ((x (walk x cx)))
    (cond
      ((vector? x) (on-vector x cx))
      ((lvar?   x) (append (let ((v (vector (new-var))))
                             (on-vector v (cx:and cx (list '= x v))))
                           (on-error (cx:and cx (list 'not (list 'has-type 'vector? x))))))
      (else        (on-error cx)))))

(define (with-ifcond x cx on-true on-false)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (append (on-true (cx:and cx (list 'not (list '= x #f))))
                         (on-false (cx:and cx (list '= x #f)))))
      (x         (on-true  cx))
      (else      (on-false cx)))))

(define (with-type t pred? x cx on-type on-not)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (append (on-type (cx:and cx (list 'has-type t x))))
                         (on-not  (cx:and cx (list 'not (list 'has-type t x)))))
      ((pred? x) (on-type cx))
      (else      (on-not cx)))))


(define (drive st)
  (let* ((frames (state-frame* st))
         (cx     (state-constraint st))
         (top    (car frames))
         (rest   (cdr frames))
         (op     (frame-op top))
         (vals   (frame-vals top))
         (env    (frame-env top)))
    (define (on-error cx) (list (state (frames-error frames) cx)))
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
            ((equal? op 'cons)
             (list (state (frames-pushval rest (cons (walk (cadr vals) cx) (walk (car vals) cx))) cx)))
            ((equal? op 'vector-ref)
             (with-vector (car vals) cx
                          (lambda (vec cx) (list (state (frames-pushval rest (vector-ref vec)) cx)))
                          on-error))
            ((assq op (map2 cons '(= symbol=? +) (list = symbol=? +)))
             => (lambda (name&proc)
                  (error "todo")
                  ))
            ((assq op (map2 cons '(car cdr) (list car cdr)))
             => (lambda (name&proc)
                  (let ((val  (car vals))
                        (proc (cdr name&proc)))
                    (with-pair val cx
                               (lambda (val cx) (list (state (frames-pushval rest (proc val)) cx)))
                               on-error))))
            ((symbol=? op 'vector)
             (list (state (frames-pushval rest (vector (walk (car vals) cx))) cx)))
            ((assq op
                   (map2 cons
                         '(null? boolean? pair? number? symbol? procedure? vector?)
                         (list null? boolean? pair? number? symbol? procedure? vector?)))
             => (lambda (name&proc)
                    (with-type (car vals)
                               (car name&proc)
                               (cdr name&proc)
                               (lambda (cx) (list (state (frames-pushval rest #t) cx)))
                               (lambda (cx) (list (state (frames-pushval rest #f) cx))))))
            (else (error "invalid frame op" top))))))
      ((not (pair? op)) (error "invalid frame op" top))
      (else
       (case (car op)
         ((lookup) (list (state (frames-pushval rest (env-ref env (op:lookup-v op))) cx)))
         ((quote)  (list (state (frames-pushval rest (quote-a op)) cx)))
         ((if) (with-ifcond
                 (car vals)
                 cx
                 (lambda (cx) (list (state (expr->frames (op:if-t op) env rest) cx)))
                 (lambda (cx) (list (state (expr->frames (op:if-f op) env rest) cx)))))
         ((lambda) (list (state (frames-pushval rest (make-closure op env)) cx)))
         ((letrec) (let ((bpair* (letrec-binding* op)))
                     (list (state (expr->frames (letrec-body op)
                                                (env-extend*/rec env (map binding-lhs bpair*) (map binding-rhs bpair*))
                                                rest)
                                  cx))))
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
