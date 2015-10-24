#lang racket

(require "common-grade.rkt")
(require "hw1-3.rkt")

(define (yanghuiGrade)
	(begin
		(printf "yanghui\n")
        (output (lambda () (equal? "11112113311464115101051161520156117213535217118285670562881" (yanghui 9))))
        (output (lambda () (equal? "111121133114641151010511615201561172135352171" (yanghui 8))))
        (output (lambda () (equal? "111121133114641151010511615201561" (yanghui 7))))
        (output (lambda () (equal? "11112113311464115101051" (yanghui 6))))
        (output (lambda () (equal? "111121133114641" (yanghui 5))))
        (output (lambda () (equal? "1111211331" (yanghui 4))))
        (output (lambda () (equal? "111121" (yanghui 3))))
        (output (lambda () (equal? "111" (yanghui 2))))
        (output (lambda () (equal? "1" (yanghui 1))))
        (output (lambda () (equal? "" (yanghui 0))))))

(yanghuiGrade)
