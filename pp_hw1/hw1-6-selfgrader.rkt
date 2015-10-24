#lang racket

(require "common-grade.rkt")
(require "hw1-6.rkt")

(define (crazy2valGrade)
	(begin
		(printf "crazy2val\n")
        (output (lambda () (equal? -1 (crazy2val 'n))))
        (output (lambda () (equal? 1 (crazy2val '(p z z . z)))))
        (output (lambda () (equal? 1 (crazy2val 'p))))
        (output (lambda () (equal? -301 (crazy2val '(p n n p p z n z n . z)))))
        (output (lambda () (equal? 9 (crazy2val '(n n p p . z)))))
        (output (lambda () (equal? -1 (crazy2val '(p p p p . n)))))
        (output (lambda () (equal? -3 (crazy2val '(n n . z)))))
        (output (lambda () (equal? 1011 (crazy2val '(p n p z p p p p p n p . z)))))
        (output (lambda () (equal? -13 (crazy2val '(n z p z p . n)))))
        (output (lambda () (equal? 549 (crazy2val '(n p p z z n n n p n p z . z)))))
))

(crazy2valGrade)
