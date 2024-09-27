#lang racket

(define expr1
  '(letrec ((f (lambda (x) x)))
     (f (quote 1))))

(define expr2
  '(letrec ((f (lambda (l)
                 (if (pair? l)
                   (cons (quote 1) 
                         (f (cdr l)))
                   (quote ())))))
     (f (quote ()))))

(define expr3
  '(letrec ((f (lambda (l)
                 (if (pair? l)
                   (cons (quote 1) 
                         (f (cdr l)))
                   (quote ())))))
     (f (cons (quote ()) (quote ())))))


