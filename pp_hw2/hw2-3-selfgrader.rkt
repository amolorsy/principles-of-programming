#lang racket

(require "common-grade.rkt")
(require "hw2-3.rkt") ; change "hw1-1.rkt" to your filename

(define c1 zero)
(define c2 one)
(define c3 (not-circuit c1))
(define c4 (and-circuit c2 c3))
(define c5 (not-circuit c4))
(define c6 (or-circuit c3 c5))


(define (circuitGrade)
  (begin
    (printf "circuit\n")
    (sgoutput (lambda () (equal? #t (is-zero? c1)) ))
    (sgoutput (lambda () (equal? #t (is-not? c3)) ))
    (sgoutput (lambda () (equal? #t (is-or? c6)) ))
    (sgoutput (lambda () (equal? #f (is-zero? c2)) ))
    (sgoutput (lambda () (equal? #f (is-and? c5)) ))          
    
    (sgoutput (lambda () (equal? c1 (sub-circuit c3 0)) ))
    (sgoutput (lambda () (equal? c3 (sub-circuit c4 1)) ))
    (sgoutput (lambda () (equal? c3 (sub-circuit c6 0)) ))
    (sgoutput (lambda () (equal? c4 (sub-circuit (sub-circuit c6 1) 0)) ))
    (sgoutput (lambda () (equal? #t (is-and? (sub-circuit (sub-circuit c6 1) 0))) ))
))

(circuitGrade)
