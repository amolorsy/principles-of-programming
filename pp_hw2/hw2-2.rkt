#lang racket
(require "hw2-1.rkt")
(provide model make-branch make-mobile)
(provide weight is-balanced?)

(define (model n)
  (leaf n)
)

(define (make-branch n m)
  (cons (model 'branch) (leaf (cons n (leaf m))))
)

(define (make-mobile b1 b2)
  (cons (leaf 'mobile) (rest (append (node (leaf b1)) (leaf b2))))
)

(define (weight m)
  (cond
    [(equal? (length m) 1) (leaf-val m)]
    [(equal? (first m) (list 'branch)) (weight (last (last m)))]
    [else (+ (weight (second m)) (weight (third m)))]
  )
)

(define (is-balanced? m)
  (cond
    [(equal? (length m) 1) #t]
    [else
     (if (= (* (first (last (second m))) (weight (last (last (second m))))) (* (first (last (third m))) (weight (last (last (third m))))))
         (and (is-balanced? (last (last (second m)))) (is-balanced? (last (last (third m))))) #f)]
  )
)