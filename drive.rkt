#lang racket/base
(provide drive)
(require "common.rkt" "step.rkt" racket/bool)
(require racket/pretty)

(module+ test
  (require rackunit))

(define (new-var)
  (lvar (gensym 'x)))


;; TODO:
;; States and residual code generation:
;; - instead of lists of states, we should use a tree structure, with nodes representing call entry points, conditional branches,
;;   let bindings, and in-progress states.  When an in-progress state becomes terminal, or folds, it will residualize an expression.
;;   - some nodes (and edges) might be labelled with introduced constraints, to inform residual code generation
;;   - unused call entries (not targeted by any folds) are transient steps, and can be pruned
;;   - during driving, let bindings bind logic variables to residual expressions
;;     - when the let binding is later residualized itself, these logic variables are turned into lexical variables
;;       - we either have stopped on a value, which generates code to build that value
;;       - or we have stopped on a logic variable, which means it has no equality constraints (it wouldn't be an lvar after walking)
;;         in which case we residualize a lexical variable, which will match the
;;         one residualized for the let lhs! (or recursive lambda parameter, in the case of folding, or normal lambda parameter
;;         for the topmost entry)
;;     - let bindings are *not* the history of delayed computations, which is a separate concept
;;     - however, unobserved history computations eventually produce let bindings
;; - we don't need "decompose" nodes due to CBV evaluation order, which causes the rhs of a let binding to already be done
;;   - and typically, constructor arguments have already been evaluated
;; - depending on the effects we support, we can (and should) safely delay evaluation of some computations by replacing
;;   them with a fresh logic variable, and inserting a "history" entry that maps this lvar to the delayed computation
;;   - computations that should definitely be delayed are primitive operations whose result is unknown, and procedure
;;     calls that trigger the "whistle" (they risk non-termination, or are otherwise judged to be not worth pursuing)
;;     - but for best results, it probably makes sense to delay everything until observation, or other guarantee of demand
;;   - if an lvar associated with a history entry is observed as the condition of a branch (by using it as the condition of
;;     an if expression, or using it in a way that could fail (requiring an implicit safety-checking branch)), we may be
;;     able to apply has-type constraints in each branch based on the suspended computation in the history entry, in
;;     addition to the == #f and =/= #f constraints applied to the lvar itself
;;   - the history corresponds to computations that will potentially become let bindings in the residual code
;;   - history entries can be removed early by participating as an argument in a fold (this is desirable)
;;   - once driving stops, we must finish driving all remaining history entries, residualizing them as let-bindings
;; - for simplificty, we can start by treating errors and non-termination as equivalent effects
;;   - under this assumption, every computation becomes eligible for delaying, aside from the caveat about effort duplication
;; Folding:
;; - we can fold when the entire current frame stack pattern-matches with a complete stack from an earlier state
;;   - if our current complete stack matches, we can stop driving and generate a (recursive) procedure call (with appropriate
;;     arguments)
;;   - we should check for folding when we are about to perform a procedure call (the operator and all operands have already been evaluated)
;; - when we are about to perform a procedure call that corresponds to a fold target, but our stack does not unify with the
;;   fold target, we should suspend the call (returning a logic variable) and continue normally
;;   - we may later be able to fold with the suspended call as a subcomponent (e.g., the (map g (cdr xs)) in (map f (map g (cdr xs))))
;;   - and if we don't manage to fold later, we will residualize a let binding for the suspended call, allowing it to be driven starting in
;;     its own empty stack, eventually allowing it to successfully fold
;;     - if a recursive procedure is not tail recursive, the recursive call will appear as an argument, which means at least one more
;;       generalization step will be required, producing another let binding that begins a call in an empty stack, finally triggering a fold
;; TODO: cx:and needs to check for inconsistency, and we need to then detect this (it can return #f)
;; - we can then prune states whose cx is #f
;;   - in the residual code, this manifests as eliminating unnecessary conditional (i.e., (if predicate? _ _)) checks
;;   - because one of the branches is known to be impossible due to the inconsistent constraint
;; - inconsistent states do NOT correspond to computing an "error"
;;   - instead, operations that can produce an error will implicitly branch with a safety check
;;     - the failure branch explicitly computes "error" and receives the appropriate negated has-type constraint
;;       - if this constraint is inconsistent, it means an "error" is not possible, because we already have evidence that the operation is safe to perform
;;     - the success branch receives the appropriate has-type constraint, allowing future safety checks to eliminate the error branch
;; TODO: to reduce the tedium, we might want to refactor the code to use a small, miniKanren-inspired formula DSL for expressing
;; constrained evaluation involving logic variables

(define (with-pair x cx on-pair on-error)
  (let ((x (walk x cx)))
    (cond
      ((pair? x) (on-pair x cx))
      ((lvar? x) (append (let ((p (cons (new-var) (new-var))))
                           (on-pair p (cx:and cx (cx:= p x))))
                         (on-error (cx:and cx (cx:not (cx:has-type 'pair? x))))))
      (else      (on-error cx)))))

;; This assumes all vectors contain a single element.
(define (with-vector x cx on-vector on-error)
  (let ((x (walk x cx)))
    (cond
      ((vector? x) (on-vector x cx))
      ((lvar?   x) (append (let ((v (vector (new-var))))
                             (on-vector v (cx:and cx (cx:= x v))))
                           (on-error (cx:and cx (cx:not (cx:has-type 'vector? x))))))
      (else        (on-error cx)))))

(define (with-= x1 x2 cx on-true on-false)
  (append (on-true  (cx:and cx (cx:= x1 x2)))
          (on-false (cx:and cx (cx:not (cx:= x1 x2))))))

(define (with-ifcond x cx on-true on-false)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (with-= x #f cx on-false on-true))
      (x         (on-true  cx))
      (else      (on-false cx)))))

(define (with-type t pred? x cx on-type on-not)
  (let ((x (walk x cx)))
    (cond
      ((lvar? x) (append (on-type (cx:and cx (cx:has-type t x))) ; make cx:has-type
                         (on-not  (cx:and cx (cx:not (cx:has-type t x))))))
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
                         ((closure? proc)
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
             (with-vector (cadr vals) cx
                          (lambda (vec cx)
                            (list (state (frames-pushval rest (vector-ref vec 0))
                                         (cx:and cx (cx:= (car vals) 0)))))
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
                  (let ((n1 (walk (cadr vals) cx))
                        (n2 (walk (car vals) cx))
                        (proc (cdr name&proc)))
                    (with-number
                      n1
                      cx
                      (lambda (cx)
                        (with-number
                          n2
                          cx
                          (lambda (cx)
                            (let* ((sum (new-var))
                                   (cx (cx:and cx (cx:+= n1 n2 sum))))
                               (list (state (frames-pushval rest sum) cx))))
                         on-error))
                    on-error))))
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
                    (with-type (car name&proc) ; name
                               (cdr name&proc) ; proc
                               (car vals)
                               cx
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
  (define (test-equal-singleton? msg frames (constraints constraint.empty))
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
  (print-and-test-sequence "drive a call" '(call (lambda (v) (quote 0)) (quote 2)) 4)
  (print-and-test-sequence "drive a call with 2 args" '(call (lambda (x y) x) (quote 1) (quote 2)) 6)
  (print-and-test-sequence "drive a cons" '(cons (quote 0) (quote 2)) 4)
  (print-and-test-sequence "drive a vector" '(vector-ref (vector (quote 2)) (quote 0)) 4)

  (print-and-test-sequence "drive a comparison #f" '(= (quote 2) (quote 0)) 4)
  (print-and-test-sequence "drive a comparison #t" '(= (quote 2) (quote 2)) 4)
  (print-and-test-sequence "drive a symbol=?" '(symbol=? (quote a) (quote a)) 4)
  (print-and-test-sequence "drive a symbol=?" '(symbol=? (quote a) (quote b)) 4)

  (print-and-test-sequence "drive a unary" '(null? (quote a)) 3)
  (print-and-test-sequence "drive a unary" '(null? '()) 3)

  (print-and-test-sequence "drive a pair? #f" '(pair? '()) 3)
  (print-and-test-sequence "drive a pair? #t" '(pair? (cons (quote 2) (quote 3))) 5)

  (print-and-test-sequence "drive an if #t" '(if (quote #t) (quote 0) (quote 1)) 4)
  (print-and-test-sequence "drive an if #f" '(if (quote #f) (quote 0) (quote 1)) 4)
  (print-and-test-sequence "drive a nested if" '(if (quote #t) (if (quote #f) (quote 0) (quote 1)) (quote 2)) 6)

  (print-and-test-sequence "drive a letrec"
                           '(letrec ((len (lambda (lst)
                                            (if (pair? lst)
                                              (cons (quote 1) (call len (cdr lst))) ; todo change cons => +
                                              (quote 0)))))
                              (call len (cons (quote 1) (cons (quote 0) (cons (quote 1) (quote ()))))))
                           42)
  (print-and-test-sequence "drive a +" '(+ (quote 2) (quote 0)) 4 #t) ; TODO
  (print-and-test-sequence "drive a +" '(+ (quote 1) (+ (quote 2) (quote 3))) 6 #t) ; TODO

  (define lvar-a (lvar 'a))
  (define lvar-frames (list `#s(frame pair? (,lvar-a) () ()) #s(frame halt () () ())))
  (define lvar-result
    (list (state (list #s(frame halt (#t) () ())) (cx:has-type 'pair? lvar-a))
          (state (list #s(frame halt (#f) () ())) (cx:not (cx:has-type 'pair? lvar-a)))))
  (test-equal?
      "drive a pair? with lvars"
      (drive (state lvar-frames constraint.empty))
      lvar-result)
)
