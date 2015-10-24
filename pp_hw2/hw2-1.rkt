#lang racket

(provide leaf node)
(provide is-leaf? leaf-val nth-child)

(define (leaf n)
  (list n)
)

(define (node l)
  (if (null? l) null (cons (leaf 'node) l))
)

(define (is-leaf? t)
  (if (= 1 (length t)) #t #f)
)

(define (leaf-val t)
  (if (is-leaf? t) (car t) (void))
)

(define (nth-child t n)
  (cond
    [(= n 0) (first (rest t))]
    [else (nth-child (rest t) (- n 1))]
  )
)