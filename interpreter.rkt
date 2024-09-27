#lang racket


(define (lookup env key)
  TODO)

(define (eval expr env)
  (if (symbol? expr)
      (lookup env expr)
      '()))


