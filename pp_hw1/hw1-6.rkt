#lang racket

(provide crazy2val)

(define (return-num i)
  (cond
    ((string=? (symbol->string i) "z") 0)
    ((string=? (symbol->string i) "p") 1)
    ((string=? (symbol->string i) "n") -1)
    (else (void))
  )
)

(define (return-2power t)
  (if (= t 0) 1 (* 2 (return-2power (- t 1))))
)

(define (crazy2val c)
  (if (pair? c) (+ (* (return-2power (add1 -1)) (return-num (car c))) (* (return-2power (add1 0)) (crazy2val (cdr c)))) (return-num c))
)