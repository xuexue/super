# A toy supercompiler

Toy supercompiler for the following language:

```
;; Atom: a null, boolean, number, or symbol.
A ::= () | #f | #t | <int-number> | <symbol>

;; Value: an atom, singleton vector, pair, or procedure (represented as a closure).
;; ENV is an environment, which can be represented in various ways
V ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ENV)

;; Lambda expression:
LAM ::= (lambda (<symbol> ...) E)

;; Expression:
E ::=
    ;; variable
    <symbol>
    ;; constructors
    | (quote A)
    | (cons E E)
    | (vector E)
    | (+ E E)
    | LAM
    ;; accessors
    | (car E)
    | (cdr E)
    | (vector-ref E E)
    ;; predicates
    | (= E E) ; fails for non-numbers
    | (symbol=? E E)
    | (null? E)
    | (boolean? E)
    | (vector? E)
    | (pair? E)
    | (number? E)
    | (symbol? E)
    | (procedure? E)
    ;; case analysis
    | (if E E E)
    ;; procedure call
    | (call E E ...)
    ;; recursive procedure binding
    | (letrec ((<symbol> LAM) ...) E)
```

The file `interpreter.rkt` contains a big step interpreter for the above language, 
intended to be easily desugared to be self-applicable


The file `step.rkt` contains a small step interpreter. Each call to `step`
with a stack of frames---which we will call a *state*--will produce a new
state, with a small bounded amount of computation performed.

```
;; Frame:
F ::= #s(frame OP (V ...) (E ...) ENV)

;; OP: 
OP ::= 
     ;; outer most context, i.e. return
     halt
     ;; variable
     | (lookup <symbol>)
     ;; constructors
     | (quote A)
     | cons
     | vector
     | +
     | LAM
     ;; accessors
     | car
     | cdr
     | vector-ref
     ;; predicates
     | =
     | symbol=?
     | null?
     | boolean?
     | pair?
     | number?
     | symbol?
     | procedure?
     | vector?
     ;; case analysis
     | (if E E)
     ;; procedure call
     | call
     ;; recursive procedure binding
     | (letrec ((<symbol> LAM) ...) E)

;; Stack : (with added invariant below)
STACK :: = (F F ...)
```

A stack follows the invariant that the top most frame can always
immediately return a value, without having to generate new frames first---
that is, it has no subexpressions remaining to be evaluated.
A stack also has the `halt` frame  in the outer most context, i.e. at the bottom.


The file `drive.rkt` contains a driver, that takes an expansion step to produce
the next *state*. The driver takes:

- state: STATE below
- rho: a memoization list (to be defined)
- (maybe a list of *logic variables*? the unbound/open variables in the expression being evaluated?

The drive produces a value of type `NODE`.

```
;; Value: redefinition of a value to also include a *logic variable*
V  ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ENV) | LV

;; Logic Variable
LV    ::= #s(lvar <symbol>)

;; types
TYPE ::= null? | num? | pair? | symbol? | boolean? | procedure? | vector?

;; state
STATE ::= (state STACK C)

;; driving nodes
N ::= done | error | (transient STATE)
    | (canfail C STATE)
    | (if V STATE STATE)
    | (decompose ???)
    | (call name args STATE)  ; name is freshly generated (h0, h1 etc.)
    | (fold target-name arg-substitution)  ; arg-substitution formed by MSG of the args for both target call and fold call
```

Constraints are described below

## Operations on Constraints

Constraint representation

```
;; constraint
CX ::= (has-type TYPE) | (not-type TYPE) | (= V) | (not-= V)
C  ::= { LV : (CX CX ...) }  ;; implicitly a conjunction of these things
```

Constraint operations

```
; adding of new constraints, with possible failure
(cx-add CX C)


; combine two sets of constraint-stores, arising from different frames.
; e.g. in an expression (+ E1 E2), evaluating E1 produces C1, and E2 => C2
(cx-combine C1 C2)
; ==> how would this work with potentially different logic variable names?


; check if two sets of constraints are identical
; "with respect to the remaining computation" (whatever that means)
; "obsevably equivalent"
;   maybe don't need this? check equality w.r.t. some reified thingy?
(cx-= C1 C2)

```

- smartly add new constraints (but can fail if the new constraint
  does not mesh well with the old ones).
  this is like unification?

## Next Steps

- port existing code to these new types
