#lang racket

(provide zipper)

(define zipper
  (lambda (first-lst second-lst)
    (cond
      ((empty? first-lst) second-lst)
      ((empty? second-lst) first-lst)
      (else (cons (first first-lst) (cons (first second-lst) (zipper (rest first-lst) (rest second-lst)))))
    )
  )
)