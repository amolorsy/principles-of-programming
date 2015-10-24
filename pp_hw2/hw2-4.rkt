#lang racket
(require "hw2-3.rkt")
(provide output)

(define (not-calculate n)
  (if (= n 0) 1 0)
)

(define (output c)
  (cond
    [(equal? c zero) 0]
    [(equal? c one) 1]
    [else
     (cond
       [(equal? (first c) (list 'and)) (and (output (second c)) (output (third c)))]
       [(equal? (first c) (list 'or)) (or (output (second c)) (output (third c)))]
       [(equal? (first c) (list 'not)) (not-calculate (output (last c)))]
     )]
  )
)