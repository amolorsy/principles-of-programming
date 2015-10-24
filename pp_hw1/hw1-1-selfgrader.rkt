#lang racket

(require "common-grade.rkt")
(require "hw1-1.rkt") ; change "hw1-1.rkt" to your filename

(define (gcdGrade)
  (begin
    (printf "gcd\n")
    (output (lambda () (equal? 8 (gcd 72 112))))
    (output (lambda () (equal? 7 (gcd 28 133))))
    (output (lambda () (equal? 9 (gcd 99 90))))
    (output (lambda () (equal? 1 (gcd 1 9))))
    (output (lambda () (equal? 4 (gcd 36 80))))
    (output (lambda () (equal? 5 (gcd 45 70))))
    (output (lambda () (equal? 2 (gcd 34 4))))
    (output (lambda () (equal? 5 (gcd 5 0))))
    (output (lambda () (equal? 9 (gcd 0 9))))
    (output (lambda () (equal? 0 (gcd 0 0))))
))

(gcdGrade)
