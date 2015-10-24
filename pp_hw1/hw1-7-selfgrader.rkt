#lang racket

(require "common-grade.rkt")
; this grader works only if you did hw1-6 correctly.
; However, for actual grading we will use the correct answer of hw1-6.
; So you will get proper grade even if you don't do hw1-6.
(require "hw1-6.rkt")
(require "hw1-7.rkt")

(define (crazy2addGrade)
	(begin
		(printf "crazy2add\n")
        (output (lambda () (equal? 0 (crazy2val (crazy2add 'z 'z)))))
        (output (lambda () (equal? 0 (crazy2val (crazy2add 'n 'p)))))
        (output (lambda () (equal? 1 (crazy2val (crazy2add 'z 'p)))))
        (output (lambda () (equal? 4 (crazy2val (crazy2add '(p . p) 'p)))))
        (output (lambda () (equal? 2342 (crazy2val (crazy2add '(p n n p p p z z p z z n . p) '(p n n . n))))))
        (output (lambda () (equal? 16731 (crazy2val (crazy2add '(z n z p n n n n z n n n p z n . p) '(p z n n p z n n . p))))))
        (output (lambda () (equal? -2120 (crazy2val (crazy2add '(n n n n z z p n z n n p z n n z . p) '(n z z n n n n n p n z p p z n . n))))))
        (output (lambda () (equal? -683 (crazy2val (crazy2add '(n z z . z) '(z p p z n p p z p z . n))))))
        (output (lambda () (equal? -37932 (crazy2val (crazy2add '(p p n z p z p p p n p n p p n . n) '(p z n n n p . z))))))
        (output (lambda () (equal? 37090 (crazy2val (crazy2add '(n z . p) '(p p p p p z p n n n p z n p z . p))))))
))

(crazy2addGrade)