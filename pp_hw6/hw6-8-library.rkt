#lang racket

(provide mustItems mustBeTheSame mustHaveExceptFor mustHaveCommon mustAnd)
(provide isItems isSame isExcept isCommon isAnd)
(provide whichItems whoTheSame condExcept itemsExcept condCommon condAnd)

(define (mustItems gifts)
  (list 'items gifts))

(define (mustBeTheSame id)
  (list 'same id))

(define (mustHaveExceptFor cond gifts)
  (list 'except cond gifts))

(define (mustHaveCommon lhs rhs)
  (list 'common (cons lhs rhs)))

(define (mustAnd lhs rhs)
  (list 'and (cons lhs rhs)))

(define (isItems cond)
  (equal? 'items (car cond)))

(define (isSame cond)
  (equal? 'same (car cond)))

(define (isExcept cond)
  (equal? 'except (car cond)))

(define (isCommon cond)
  (equal? 'common (car cond)))

(define (isAnd cond)
  (equal? 'and (car cond)))

(define (whichItems cond)
  (cadr cond))

(define (whoTheSame cond)
  (cadr cond))

(define (condExcept cond)
  (cadr cond))

(define (itemsExcept cond)
  (caddr cond))

(define (condCommon cond)
  (cadr cond))

(define (condAnd cond)
  (cadr cond))
