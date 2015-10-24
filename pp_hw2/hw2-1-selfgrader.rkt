#lang racket

(require "common-grade.rkt")
(require "hw2-1.rkt") ; change "hw1-1.rkt" to your filename

(define testtree0
  (node (list (leaf 1))))
(define testtree1
  (node (list testtree0 (leaf 2))))
(define testtree2
  (node (list testtree0 testtree1 testtree0 (leaf 6))))

(define (treeGrade)
  (begin
    (printf "tree\n")
    (sgoutput (lambda () (equal? #t (is-leaf? (leaf '(1 2)))) ))
    (sgoutput (lambda () (equal? #t (is-leaf? (leaf (node (list (leaf 1)))))) ))
    (sgoutput (lambda () (equal? 'val (leaf-val (leaf 'val))) ))
    (sgoutput (lambda () (equal? (leaf 2) (nth-child testtree1 1)) ))
    (sgoutput (lambda () (equal? testtree0 (nth-child testtree1 0)) ))
    (sgoutput (lambda () (equal? #t (is-leaf? (nth-child (nth-child testtree1 0) 0))) ))
    (sgoutput (lambda () (equal? 1 (leaf-val (nth-child (nth-child testtree1 0) 0))) ))
    (sgoutput (lambda () (equal? (leaf 6) (nth-child testtree2 3)) ))
    (sgoutput (lambda () (equal? #f (is-leaf? (nth-child (nth-child testtree2 1) 0))) ))
    (sgoutput (lambda () (equal? (leaf 1) (nth-child (nth-child (nth-child testtree2 1) 0) 0)) ))
))

(treeGrade)
