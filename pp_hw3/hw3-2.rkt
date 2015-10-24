#lang racket

(require "hw3-2-library.rkt")
(provide maze-check)

;(maze-check (cons 4
;        '((1 4) (0 5) (3 6) (2 7)
;          (0 5 8) (1 4) (2) (3)
;          (4 12) (10 13) (9 11 14) (10)
;          (8 13) (9 12) (10 15) (14))) 4 7)

(define (maze-solving maze start end visited stack)
  (cond
    [(equal? start end) #t]
    [(and (empty? stack) (is-subset? (can-enter maze start) visited)) #f]
    [(is-subset? (can-enter maze start) visited) (maze-solving maze (last stack) end visited (remove (last stack) stack))] 
    [else (for/first ([i (can-enter maze start)] #:when (not (is-member? i visited)))
            (maze-solving maze i end (append visited (list i)) (append stack (list start))))
    ]
  )
)

(define (maze-check maze start end)
  (maze-solving maze start end (list start) empty-set)
)