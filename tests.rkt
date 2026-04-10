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

(define expr4
  '(letrec ((s-map (lambda (f x*)
                     ((letrec ((loop (lambda (x*)
                                       (let ((x* (x*)))
                                         (if (null? x*)
                                             (lambda () (quote ()))
                                             (lambda () (cons (f (car x*))
                                                              (loop (cdr x*)))))))))
                        loop)
                      x*))))
     (lambda (x*)
       (s-map (lambda (x) (+ (+ x x) x))
              (s-map (lambda (x) (+ x 1)) x*)))))

(define expr5
  '((letrec ((s-map (lambda (f x*)
                      ((letrec ((loop (lambda (x*)
                                        (let ((x* (x*)))
                                          (if (null? x*)
                                              (lambda () (quote ()))
                                              (lambda () (cons (f (car x*))
                                                               (loop (cdr x*)))))))))
                         loop)
                       x*))))
      (lambda (x*)
        (s-map (lambda (x) (+ (+ x x) x))
               (s-map (lambda (x) (+ x 1)) x*))))
    (lambda () (cons 1 (lambda () (cons 2 (lambda () (cons 3 (lambda () (quote ()))))))))))

(define expr6
  '(letrec ((map (lambda (f x*)
                   ((letrec ((loop (lambda (x*)
                                     (if (null? x*)
                                         (quote ())
                                         (cons (f (car x*))
                                               (loop (cdr x*)))))))
                      loop)
                    x*))))
     (lambda (x*)
       (map (lambda (x) (+ (+ x x) x))
            (map (lambda (x) (+ x 1)) x*)))))

(define expr7
  '((letrec ((map (lambda (f x*)
                    ((letrec ((loop (lambda (x*)
                                      (if (null? x*)
                                          (quote ())
                                          (cons (f (car x*))
                                                (loop (cdr x*)))))))
                       loop)
                     x*))))
      (lambda (x*)
        (map (lambda (x) (+ (+ x x) x))
             (map (lambda (x) (+ x 1)) x*))))
    (cons 1 (cons 2 (cons 3 (quote ()))))))
