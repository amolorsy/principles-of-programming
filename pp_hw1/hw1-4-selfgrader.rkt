#lang racket

(require "common-grade.rkt")
(require "hw1-4.rkt")

(define (zipperGrade)
	(begin
		(printf "t2\n")
        (output (lambda () (equal? '() (zipper '() '()))))
        (output (lambda () (equal? '(1 4 3 2) (zipper '(1 3 2) '(4)))))
        (output (lambda () (equal? '(1 5 2 6 3 4) (zipper '(1 2 3 4) '(5 6)))))
        (output (lambda () (equal? '(1 4 2) (zipper '() '(1 4 2)))))
        (output (lambda () (equal? '(5 3) (zipper '(5 3) '()))))
        (output (lambda () (equal? '(1 2) (zipper '(1) '(2)))))
        (output (lambda () (equal? '(5) (zipper '(5) '()))))
        (output (lambda () (equal? '(3 0 7 3 4 5) (zipper '(3 7 4) '(0 3 5)))))
        (output (lambda () (equal? '(1 3 5 7) (zipper '(1 3 5 7) '()))))
        (output (lambda () (equal? '(16 2 3 4 5 6 7 8) (zipper '(16 3 5 7) '(2 4 6 8))   )))
))

(zipperGrade)
