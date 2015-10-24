#lang racket

(require "common-grade.rkt")
(require "hw1-5.rkt")

(define (zipperNGrade)
	(begin
		(printf "t2\n")
        (output (lambda () (equal? '() (zipperN '(() ())) )))
        (output (lambda () (equal? '() (zipperN '(() () ())) )))
        (output (lambda () (equal? '(1 4 7 2 5 8 3 6 9) (zipperN '((1 2 3) (4 5 6) (7 8 9))))))
        (output (lambda () (equal? '(1 2 10 3 8 20 5 30 7) (zipperN '((1 3 5 7) (2 8) () (10 20 30))))))
        (output (lambda () (equal? '(2 4 5) (zipperN '(() () (2 4 5) ())))))
        (output (lambda () (equal? '(2 7 4 8 5 9) (zipperN '((2 4 5) (7 8 9))))))
        (output (lambda () (equal? '(1 2 3 4 5 6) (zipperN '(() (1 3 5) () (2 4 6) ())))))
        (output (lambda () (equal? '(1 2 3 6 5 4) (zipperN '((1) (2) (3) (6) (5) (4))))))
        (output (lambda () (equal? '(-2 -1 3 -3 6 -5) (zipperN '((-2 3 6) (-1 -3 -5))))))
        (output (lambda () (equal? '(2 3 4 2 3 4 2 3 4 3 4 4) (zipperN '((2 2 2) (3 3 3 3) (4 4 4 4 4))) )))
))

(zipperNGrade)
