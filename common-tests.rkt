#lang racket/base
(require rackunit)
(require "common.rkt")

(define l1 (lvar 'x))
(define cx1 (cx:has-type 'number?))
(define cx2 (cx:not-type 'number?))
(define cx3 (cx:has-type 'boolean?))
(define cx4 (cx:not-type 'boolean?))
(define cx5 (cx:= 42))
(define cx6 (cx:not-= 42))
(define cx7 (cx:= 99))
(define cx8 (cx:not-= 99))
(define cx9 (cx:= #t))
(define cx10 (cx:has-type 'symbol?))
(define cx11 (cx:= 'foo))

;; has-type vs not-type (conflict)
(check-true (cx-conflicts? (list cx2) cx1))
(check-true (cx-conflicts? (list cx1) cx2))
(check-false (cx-conflicts? (list cx4) cx1))
(check-false (cx-conflicts? (list cx3) cx2))

;; = vs not-= (conflict)
(check-true (cx-conflicts? (list cx6) cx5))
(check-true (cx-conflicts? (list cx5) cx6))
(check-false (cx-conflicts? (list cx8) cx5))
(check-false (cx-conflicts? (list cx7) cx6))

;; No conflicts with unrelated constraints
(check-false (cx-conflicts? (list cx7) cx1))
(check-true (cx-conflicts? (list cx5) cx3))
(check-false (cx-conflicts? '() cx1))

;; has-type vs = (type mismatch)
(check-true (cx-conflicts? (list cx1) cx9)) ; cx1: number?, cx9: = #t (boolean)
(check-true (cx-conflicts? (list cx3) cx5)) ; cx3: boolean?, cx5: = 42 (number)
(check-true (cx-conflicts? (list cx10) cx5)) ; cx10: symbol?, cx5: = 42 (number)
(check-true (cx-conflicts? (list cx1) cx11)) ; cx1: number?, cx11: = 'foo (symbol)

;; = vs has-type (type mismatch, reverse order)
(check-true (cx-conflicts? (list cx9) cx1)) ; cx9: = #t, cx1: number?
(check-true (cx-conflicts? (list cx5) cx3)) ; cx5: = 42, cx3: boolean?
(check-true (cx-conflicts? (list cx5) cx10)) ; cx5: = 42, cx10: symbol?
(check-true (cx-conflicts? (list cx11) cx1)) ; cx11: = 'foo, cx1: number?

;; has-type vs = (no conflict)
(check-false (cx-conflicts? (list cx1) cx5)) ; cx1: number?, cx5: = 42
(check-false (cx-conflicts? (list cx3) cx9)) ; cx3: boolean?, cx9: = #t
(check-false (cx-conflicts? (list cx10) cx11)) ; cx10: symbol?, cx11: = 'foo

;; has-type vs has-type (incompatible types)
(check-true (cx-conflicts? (list cx1) cx3)) ; number? vs boolean?
(check-true (cx-conflicts? (list cx3) cx10)) ; boolean? vs symbol?
(check-true (cx-conflicts? (list cx10) cx1)) ; symbol? vs number?

;; has-type vs has-type (compatible types)
(check-false (cx-conflicts? (list cx1) (cx:has-type 'number?)))
(check-false (cx-conflicts? (list cx3) (cx:has-type 'boolean?)))

;; = vs = (incompatible equalities)
(check-true (cx-conflicts? (list cx5) cx7)) ; 42 vs 99
(check-true (cx-conflicts? (list cx9) cx5)) ; #t vs 42
(check-true (cx-conflicts? (list cx11) cx5)) ; 'foo vs 42

;; = vs = (compatible equalities)
(check-false (cx-conflicts? (list cx5) (cx:= 42)))
(check-false (cx-conflicts? (list cx11) (cx:= 'foo))) 
