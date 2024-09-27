# A toy supercompiler

Grammar

```
;; Atom: a null, boolean, number, or symbol.
A ::= () | #f | #t | <int-number> | <symbol>

;; Value: an atom, singleton vector, pair, or procedure (represented as a closure).
V ::= A | #(V) | (V . V) | #s(closure (<symbol> ...) E ((<symbol> . V) ...))

;; Lambda expression:
LAM ::= (lambda (<symbol> ...) E)

;; Expression:
E ::=
    ;; variable
    <symbol>
    ;; constructors
    | (quote A)
    | (cons E E)
    | LAM
    ;; accessors
    | (car E)
    | (cdr E)
    ;; predicates
    | (atom=? E E)
    | (null? E)
    | (boolean? E)
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

;; Frame:
F ::=
    ;; constructors
    | (cons _ E)
    | (cons V _)
    ;; accessors
    | (car _)
    | (cdr _)
    ;; predicates
    | (atom=? _ E)
    | (atom=? V _)
    | (null? _)
    | (boolean? _)
    | (pair? _)
    | (number? _)
    | (symbol? _)
    | (procedure? _)
    ;; case analysis
    | (if _ E E)
    ;; procedure call
    | (call _ E ...)
    | (call V V ... _ E ... )
    ;; recursive procedure binding
    | (letrec ((<symbol> LAM) ...) _)
```
