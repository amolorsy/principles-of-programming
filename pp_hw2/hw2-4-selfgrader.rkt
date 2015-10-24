#lang racket

(require "common-grade.rkt")
(require "hw2-3.rkt")
(require "hw2-4.rkt")

(define c1 zero)
(define c2 one)
(define c3 (not-circuit c1))
(define c4 (and-circuit c2 c3))
(define c5 (not-circuit c4))
(define c6 (or-circuit c3 c5))


(define (circuitoutputGrade)
  (begin
    (printf "circuit\n")
    (sgoutput (lambda () (equal? 0 (output c1)) ))
    (sgoutput (lambda () (equal? 1 (output c2)) ))
    (sgoutput (lambda () (equal? 1 (output c3)) ))
    (sgoutput (lambda () (equal? 1 (output c4)) ))
    (sgoutput (lambda () (equal? 0 (output c5)) ))          
    (sgoutput (lambda () (equal? 1 (output c6)) ))
))

(circuitoutputGrade)
