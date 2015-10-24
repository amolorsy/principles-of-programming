#lang racket

(provide gcd)

(define (gcd n m)
  (if (equal? m 0) n
      (gcd m (remainder n m))
  )
)
