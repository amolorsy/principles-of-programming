#lang racket

;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

; If these statements are omitted, your submission will be graded 0.
(provide catchYou)

; catchYou: graph * int -> (store X real) list
;
; This task is a challenge, so TAs will test with large input
; cases. Only reasonably efficient algorithm will pass test cases.
;
; Output should be ordered by the stores' name. Note that stores' name
; are one of: 'A, 'B, 'C, 'D, or 'E.
;
; Only stores appeared in the input are considered as the starting
; point, and those only should be output.
(define (catchYou model step)
  (define (get-all-shop graph lst)
    (if (empty? graph) lst
        (let ([shop (caar graph)] [rest (cdr graph)])
          (if (member shop lst)
              (get-all-shop rest lst)
              (get-all-shop rest (append lst (list shop)))))))
  (define (get-begin-prob lst)
    (let ([prob (/ 1 (length lst))])
      (map (lambda (shop) (cons shop prob)) lst)))
  (define init-prob (get-begin-prob (get-all-shop model null)))
  (define all-shop-list (get-all-shop model null))
  (define all-shop-num (length all-shop-list))
  (define (make-matrix src)
    (define lst null)
    (for ([i all-shop-list])
      (for ([j all-shop-list])
        (let ([arrow (findf (lambda (z) (and (equal? (first z) i) (equal? (second z) j))) src)])
          (set! lst (append lst (list (list i j (if (equal? arrow #f) 0 (third arrow))))))))
    )
    lst)
  (define init-graph (make-matrix model))
  (define (get-position lst obj n)
    (cond
      [(empty? lst) 0]
      [(equal? (car lst) obj) n]
      [else (get-position (cdr lst) obj (+ n 1))])) 
  (define (calculate srcA srcB start end)
    (define value 0)
    (for ([i all-shop-num]) (set! value (+ value (* (third (list-ref srcA (+ (* all-shop-num (- (get-position all-shop-list start 1) 1)) i)))
                               (third (list-ref srcB (+ (* all-shop-num i) (- (get-position all-shop-list end 1) 1))))))))
    value)
  (define (matrix-mul srcA srcB)
    (define lst null)
    (for ([i all-shop-list])
      (for ([j all-shop-list])
        (set! lst (append lst (list (list i j (calculate srcA srcB i j)))))))
    lst)
  (define (after-n n mat)
    (if (= n 1) mat
        (if (even? n)
            (after-n (/ n 2) (matrix-mul mat mat))
            (matrix-mul (after-n (- n 1) mat) init-graph))))
  (define (last-prob prob-list obj value)
    (if (empty? prob-list) (cons obj value)
        (let ([arrow (car prob-list)])
          (if (zero? (third arrow)) (last-prob (cdr prob-list) obj value)
              (if (equal? (second arrow) obj)
                  (last-prob (cdr prob-list) obj
                             (+ value (* (third arrow) (cdr (findf (lambda (x) (equal? (car x) (first arrow))) init-prob)))))
                  (last-prob (cdr prob-list) obj value))))))
  (map (lambda (x) (cons (car x) (* (cdr x) 100))) (build-list all-shop-num (lambda (y) (last-prob (after-n step init-graph) (list-ref all-shop-list y) 0))))
)