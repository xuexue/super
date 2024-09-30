#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar (gensym 'x)))

;; TODO:
(define (walk x cx) x)

;; TODO: for faster residual code generation, we should attach labels to states indicating which logic variables and
;; constraints that were introduced (in simplified/walked form according to existing constraints)
;; - this is clearer than fishing around in the cx to figure out what changed relative to the parent state
;; - we can then generate code based on the label alone
;; - when the label is blank, this indicates that a transient step has been taken, which can be pruned
;; - labels can also include let expressions
;;   - so we just continue with one state corresponding to the let body
;;   - no actual lexical variables need to be bound: maybe surprisingly, the let is binding a logic variable on its lhs
;;   - this is less surprising when you consider how residual code is generated:
;;     - we either have stopped on a value, which generates code to build that value
;;     - or we have stopped on a logic variable, which means it has no equality constraints (it wouldn't be an lvar after walking)
;;       in which case we residualize a lexical variable, which will match the
;;       one residualized for the let lhs! (or recursive lambda parameter, in the case of folding, or normal lambda parameter
;;       for the topmost entry)
;;   - we don't need "decompose" nodes due to CBV evaluation order, which causes the rhs of a let binding to already be done
;;     - and typically, constructor arguments have already been evaluated
;;     - however, for better results (as achieved by supercompilation for CBN evaluation), we may want to delay evaluation
;;     - depending on the effects we support, we can safely delay evaluation of some subcomputations by replacing them with
;;       a fresh logic variable, and inserting an entry that maps this lvar to the postponed computation, likely represented
;;       as a frame stack
;;       - once driving stops, we must finish evaluating all remaining delayed computations
;;       - additionally, any time the value of such an lvar is observed (e.g., by an accessor, predicate, if condition), we
;;         resume the delayed computation by pushing it on top of the current stack in place of the lvar
;;       - if we are not careful, there is some risk of this lvar being duplicated, which could increase the amount of work
;;         done by the residual program
;;       - so we may want to limit delaying to situations where references of the lvar would be linear (or affine)
;;         - or, limit delaying in this way if the computation is expensive
;;         - duplicating an inexpensive computation could be acceptable
;;     - for simplificty, we can start by treating errors and non-termination as equivalent effects
;;       - under this assumption, every computation becomes eligible for delaying, aside from the caveat about effort duplication
;; Folding:
;; - we can fold when a prefix of our current frame stack pattern-matches with a complete stack from an earlier state
;;   - if our current complete stack matches, we can stop driving and generate a (recursive) procedure call (with appropriate
;;     arguments)
;;   - more generally, if only a proper prefix of the current stack matches, we have to generate a let binding for the
;;     procedure call, and continue driving the suffix frames as the body
;;   - so ideally we would try to match the largest prefix we can first
;;   - if we allow delayed computations, as mentioned in an earlier note, we should often be able to match the entirety of
;;     our current stack
;; TODO: cx:and needs to check for inconsistency, and we need to then detect this (it can return #f)
;; - we can then prune states whose cx is #f
;;   - in the residual code, this manifests as eliminating unnecessary conditional (i.e., (if predicate? _ _)) checks
;;   - because one of the branches is known to be impossible due to the inconsistent constraint
;; TODO: to reduce the tedium, we might want to refactor the code to use a small, miniKanren-inspired formula DSL for expressing
;; constrained evaluation involving logic variables

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

(define (with-= x1 x2 cx on-true on-false)
  (append (on-true  (cx:and cx (list '= x1 x2)))
          (on-false (cx:and cx (list 'not (list '= x1 x2))))))

(define (with-ifcond x cx on-true on-false)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (with-= x #f cx on-false on-true))
      (x         (on-true  cx))
      (else      (on-false cx)))))

(define (with-type t pred? x cx on-type on-not)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (append (on-type (cx:and cx (list 'has-type t x))))
                         (on-not  (cx:and cx (list 'not (list 'has-type t x)))))
      ((pred? x) (on-type cx))
      (else      (on-not cx)))))

(define (with-number x cx on-number on-not) (with-type 'number? number? x cx on-number on-not))
(define (with-symbol x cx on-symbol on-not) (with-type 'symbol? symbol? x cx on-symbol on-not))

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
            ((equal? op '=)
             (let ((n1 (walk (cadr vals) cx)) (n2 (walk (car vals) cx)))
               (with-number
                n1
                cx
                (lambda (cx)
                  (with-number
                   n2
                   cx
                   (lambda (cx)
                     (if (or (lvar? n1) (lvar? n2))
                         (with-= n1 n2 cx
                                 (lambda (cx) (list (state (frames-pushval rest #t) cx)))
                                 (lambda (cx) (list (state (frames-pushval rest #f) cx))))
                         (list (state (frames-pushval rest (= n1 n2)) cx))))
                   on-error))
                on-error)))
            ((equal? op 'symbol=?)
             (let ((s1 (walk (cadr vals) cx)) (s2 (walk (car vals) cx)))
               (with-symbol
                s1
                cx
                (lambda (cx)
                  (with-symbol
                   s2
                   cx
                   (lambda (cx)
                     (if (or (lvar? s1) (lvar? s2))
                         (with-= s1 s2 cx
                                 (lambda (cx) (list (state (frames-pushval rest #t) cx)))
                                 (lambda (cx) (list (state (frames-pushval rest #f) cx))))
                         (list (state (frames-pushval rest (symbol=? s1 s2)) cx))))
                   on-error))
                on-error)))
            ((assq op (map2 cons '(+) (list +)))
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
