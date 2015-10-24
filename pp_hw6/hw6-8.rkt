#lang racket

(require "hw6-8-library.rkt")

;;; If these statements are omitted, your submission will be graded 0.
(provide shoppingList)

;;; This problem has restrictions on the interface of shoppingList.
;;; 1. All ids are symbols consisting of capital alphabets ('A, 'B, 'DFSF, ...).
;;; 2. All gifts are integers (1, 2, 3, 100, ...).
;;; 3. The output of shoppingList should be sorted by dictionary order of id.
;;;    For example, A < AA < AB < B.
;;; 4. Each gift list in the output of shoppingList should be sorted by increasing order.
;;;
;;; See hw6-8-grade.rkt for a grading example.
;;;

(define (sort-id lst)
  (sort lst #:key car string<=?))
(define (sort-gift lst)
  (if (empty? lst) lst 
      (sort lst <)))
(define (union l r)
  (append l (let ([l-r-intersection (filter (lambda (x) (member x l)) r)])
              (filter (lambda (y) (not (member y l-r-intersection))) r))))
(define (subtraction obj target)
  (remove* obj target))
(define (intersection l r)
  (filter (lambda (x) (member x r)) l))
  
(define (shoppingList conditions) ; (id X cond) list -> (id X gift list) list
  (define gifts (sort-id (map (lambda (x) (cons (symbol->string (car x)) null)) conditions)))
  (define (shopping-once x)
    (define (get-gifts id cont)
      (cond
        [(isItems cont) (whichItems cont)]
        [(isSame cont) (cdr (findf (lambda (x) (equal? (car x) (symbol->string (whoTheSame cont)))) gifts))]
        [(isAnd cont) (union (get-gifts id (car (condAnd cont))) (get-gifts id (cdr (condAnd cont))))]
        [(isExcept cont) (subtraction (get-gifts id (itemsExcept cont)) (get-gifts id (condExcept cont)))]
        [(isCommon cont) (intersection (get-gifts id (cdr (condCommon cont))) (get-gifts id (car (condCommon cont))))]
        [else cont]))
    (for ([i conditions]) (set! gifts (sort-id (append (remove (findf (lambda (x) (equal? (car x) (symbol->string (car i)))) gifts) gifts)
                                    (list (cons (symbol->string (car i)) (sort-gift (get-gifts (symbol->string (car i)) (cdr i)))))))))
    gifts)
  (define (shopping-iter prev)
    (let ([now (shopping-once conditions)])
      (if (equal? prev now) (map (lambda (x) (cons (string->symbol (car x)) (cdr x))) prev) (shopping-iter now))))
  (shopping-iter (shopping-once conditions)))