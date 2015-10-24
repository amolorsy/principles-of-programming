#lang racket


;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)


; You can use the definitions and functions defined in hw5-1.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-1.rkt")


;;; interfaces
(define (equal f g) ; equal: form * form -> bool
  (cond
    [(and (not (list? f)) (not (list? g))) (if (equal? f g) #t #f)]
    [(and (is-tree? f) (is-tree? g)) (if (equal? f g) #t #f)]
    [(and (is-tree? f) (is-array? g)) (if (equal? f (array-to-tree g)) #t #f)]
    [(and (is-array? f) (is-tree? g)) (if (equal? f (tree-to-array g)) #t #f)]
    [else (if (equal? f g) #t #f)]))

(define (size f) ; size: form -> int
  (define (get-num axis-size)
    (if (= axis-size 1) 0 (+ 1 (get-num (/ axis-size 2))))) 
  (cond
    [(and (is-tree? f) (not (list? f))) 0]
    [(and (is-tree? f) (list? f)) (get-num (length (first (cdr (tree-to-array f)))))]
    [else (get-num (length (first (cdr f))))]))

(define (beautiful f) ; beautiful: form -> bool
  (define (beautiful-sym f)
    (define row-num (length (cdr f)))
    (if (and (equal? (rotate (rotate (cons 'array (map (lambda (lst-nw) (take lst-nw (/ (length lst-nw) 2))) (take (cdr f) (/ row-num 2))))))
                     (cons 'array (map (lambda (lst-se) (take-right lst-se (/ (length lst-se) 2))) (take-right (cdr f) (/ row-num 2)))))
             (equal? (rotate (rotate (cons 'array (map (lambda (lst-ne) (take-right lst-ne (/ (length lst-ne) 2))) (take (cdr f) (/ row-num 2))))))
                     (cons 'array (map (lambda (lst-sw) (take lst-sw (/ (length lst-sw) 2))) (take-right (cdr f) (/ row-num 2))))))
        #t #f)
  )
  (define (beautiful-neighbor f)
    (define (neighbor-search lst tile)
      (cond
        [(= (size tile) 1) (let ([nw (neighbor (append lst (list 0)) f)]
                                 [ne (neighbor (append lst (list 1)) f)]
                                 [se (neighbor (append lst (list 2)) f)]
                                 [sw (neighbor (append lst (list 3)) f)])
                          (if (and
                               (and (<= 2 nw) (>= 5 nw))
                               (and (<= 2 ne) (>= 5 ne))
                               (and (<= 2 se) (>= 5 se))
                               (and (<= 2 sw) (>= 5 sw))) #t #f))]
        [else
         (define row-num (length (cdr tile)))
         (and
          (neighbor-search (append lst (list 0)) (cons 'array (map (lambda (lst-nw) (take lst-nw (/ (length lst-nw) 2))) (take (cdr tile) (/ row-num 2)))))
          (neighbor-search (append lst (list 1)) (cons 'array (map (lambda (lst-ne) (take-right lst-ne (/ (length lst-ne) 2))) (take (cdr tile) (/ row-num 2)))))
          (neighbor-search (append lst (list 2)) (cons 'array (map (lambda (lst-se) (take-right lst-se (/ (length lst-se) 2))) (take-right (cdr tile) (/ row-num 2)))))
          (neighbor-search (append lst (list 3)) (cons 'array (map (lambda (lst-sw) (take lst-sw (/ (length lst-sw) 2))) (take-right (cdr tile) (/ row-num 2))))))]))
    (neighbor-search null f))
  (or (beautiful-sym f) (beautiful-neighbor f)))