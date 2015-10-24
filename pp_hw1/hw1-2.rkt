#lang racket

(provide t2)

(define lst '(#\0))

(define (put-one-rev n)
  (if (> n 0) (append '(#\1) (put-one-rev (- n 1)))
      lst
  )
)

(define (t2-rev n)
  (if (> n 0) (append (put-one-rev n) (t2-rev (- n 1)))
      lst
  )
)

(define (t2 n)
  (list->string (reverse (t2-rev (abs n))))
)