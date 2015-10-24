#lang racket

(require "common-grade.rkt")
(require "hw1-2.rkt")

(define (t2Grade)
	(begin
		(printf "t2\n")
        (output (lambda () (equal? "001011011101111011111011111101111111" (t2 7))))
        (output (lambda () (equal? "0010110111011110111110111111" (t2 6))))
        (output (lambda () (equal? "001011011101111011111" (t2 5))))
        (output (lambda () (equal? "001011011101111" (t2 4))))
        (output (lambda () (equal? "0010110111" (t2 3))))
        (output (lambda () (equal? "001011" (t2 2))))
        (output (lambda () (equal? "001" (t2 1))))
        (output (lambda () (equal? "0" (t2 0))))
        (output (lambda () (equal? "001" (t2 -1))))
        (output (lambda () (equal? "001011" (t2 -2))))))

(t2Grade)
