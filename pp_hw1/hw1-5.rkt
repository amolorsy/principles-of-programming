#lang racket

(provide zipperN)

(define (get-rest l)
  (cond
    [(null? l) l]
    [(null? (first l)) (get-rest (rest l))]
    [else (cons (rest (first l)) (get-rest (rest l)))]
  )
)

(define (get-first l)
  (cond
    [(null? l) l]
    [(null? (first l)) (get-first (rest l))]
    [else (cons (first (first l)) (get-first (rest l)))]
  )
)

(define (zipperN l)
  (cond
    [(null? l) l]
    [(null? (first l)) (zipperN (rest l))]
    [else (append (get-first l) (zipperN (get-rest l)))]
  )
)