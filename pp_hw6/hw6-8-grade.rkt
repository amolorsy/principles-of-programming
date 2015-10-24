#lang racket

(require "common-grade.rkt")
(require "hw6-8-library.rkt")
(require "hw6-8.rkt")

(define condA
  (mustAnd (mustItems '(1 2))
           (mustHaveCommon (mustBeTheSame 'B)
                           (mustBeTheSame 'C))))

(define condB
  (mustHaveCommon (mustBeTheSame 'C)
                  (mustItems '(2 3))))

(define condC
  (mustAnd (mustItems '(1))
           (mustHaveExceptFor (mustBeTheSame 'A)
                              '(3))))

(output
 (lambda ()
   (equal? (shoppingList
            (list (cons 'A condA) (cons 'B condB) (cons 'C condC)))
           '((A . (1 2)) (B . (2)) (C . (1 2))))))
