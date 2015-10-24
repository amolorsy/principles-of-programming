#lang racket
(require "hw2-1.rkt")
(provide zero one not-circuit and-circuit or-circuit)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero
  (leaf 0)
)

(define one
  (leaf 1)
)

(define (not-circuit c)
  (cons (leaf 'not) (leaf c))
)

(define (and-circuit c1 c2)
  (cons (leaf 'and) (rest (append (node (leaf c1)) (leaf c2))))
)

(define (or-circuit c1 c2)
  (cons (leaf 'or) (rest (append (node (leaf c1)) (leaf c2))))
)

(define (is-zero? c)
  (if (equal? c (leaf 0)) #t #f)
)

(define (is-one? c)
  (if (equal? c (leaf 1)) #t #f)
)

(define (is-not? c)
  (if (equal? (first c) (leaf 'not)) #t #f)
)

(define (is-and? c)
  (if (equal? (first c) (leaf 'and)) #t #f)
)

(define (is-or? c)
  (if (equal? (first c) (leaf 'or)) #t #f)
)

(define (sub-circuit c n)
  (cond
    [(= n 0) (first (rest c))]
    [else (sub-circuit (rest c) (- n 1))]
  )
)