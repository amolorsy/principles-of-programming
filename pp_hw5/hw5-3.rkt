#lang racket

;;; It is recommended to see hw5-2-grade.rkt before doing your HW.

; If these statements are omitted, your submission will be graded 0.
(provide init-tape read-tape write-tape move-tape-left move-tape-right print-tape)
(provide empty-ruletable add-rule make-rule match-rule)
(provide make-tm step-tm run-tm print-tm)


;;; Tapes

(define (init-tape syms) ; init-tape: symbol list -> tape
  (cons 0 syms))

(define (read-tape tape) ; read-tape: tape -> symbol
  (if (equal? tape (list 0)) "-"
      (if (< (car tape) 0) "-"
          (let ([src (list-ref (cdr tape) (car tape))])
            (if (equal? src 'Blank) "-" src)))))

(define (write-tape tape sym) ; write-tape: tape * symbol -> tape
  (let ([src (cdr tape)])
    (cond
      [(and (< (car tape) 0) (equal? (car src) 'Blank))
       (cons 0 (cons sym (cdr src)))]
      [(> (car tape) 0)
       (if (equal? (car tape) (- (length src) 1))
           (cons (car tape) (flatten (append (take src (car tape)) sym)))
           (cons (car tape) (flatten (append (flatten (append (take src (car tape)) sym)) (drop src (+ 1 (car tape)))))))]
      [else
       (cons (car tape) (cons sym (cdr src)))])))

(define (move-tape-left tape) ; move-tape-left: tape -> tape
  (let ([head (car tape)] [syms (cdr tape)])
    (if (equal? head (- (length syms) 1)) (cons (+ head 1) (append syms (list 'Blank)))
        (if (equal? (first syms) 'Blank) (cons (+ head 1) (remove (first syms) syms))
            (cons (+ head 1) syms)))))

(define (move-tape-right tape) ; move-tape-right: tape -> tape
  (let ([head (car tape)] [syms (cdr tape)])
    (if (<= head 0) (cons (- head 1) (append (list 'Blank) (cdr tape)))
        (if (equal? (last syms) 'Blank) (cons (- head 1) (remove (last syms) syms))
            (cons (- head 1) syms)))))

; Implement "tape * int -> string" instead of "tap -> void".
; The int argument is the size of printed tapes as in print-tm.
(define (print-tape tape size) ; print-tape: tape * int -> string
  (cond
    [(= size 0) (if (< (car tape) 0) "-" (list-ref (cdr tape) (car tape)))]
    [else
     (define (print-blank n)
       (cond
         [(< n 1) ""]
         [(= n 1) "-"]
         [else
          (string-append "-." (print-blank (- n 1)))]))
     (define (print-symbol lst)
       (cond
         [(empty? lst) "-"]
         [(= (length lst) 1) (if (equal? (car lst) 'Blank) "-" (car lst))]
         [else
          (string-append (string-append (if (equal? (car lst) 'Blank) "-" (car lst)) ".")
                         (print-symbol (cdr lst)))]))
     (let ([src (cdr tape)]
           [prev-blank-num (if (<= (car tape) 0) size (if (> (- size (car tape)) 0) (- size (car tape)) 0))]
           [prev (print-blank (if (<= (car tape) 0) size (- size (car tape))))]
           [mid (print-symbol (cdr tape))]
           [next-blank-num (if (<= (car tape) 0) (if (empty? (cdr tape)) size (- (+ 1 size) (length (cdr tape)))) size)]
           [next (print-blank (if (<= (car tape) 0) (if (empty? (cdr tape)) size (- (+ 1 size) (length (cdr tape)))) size))])
       (cond
         [(and (equal? prev "") (equal? next "")) (if (>= (length src) (+ (* 2 size) 1))
                                                      (print-symbol (take src (- (length src) (- (length src) (+ (* 2 size) 1)))))
                                                      mid)]
         [(and (equal? prev "") (not (equal? next "")))
          (if (>= (length src) (+ (* 2 size) 1))
              (print-symbol (take src (- (length src) (- (length src) (+ (* 2 size) 1)))))
              (string-append (string-append mid ".") next))]
         [(and (not (equal? prev "")) (equal? next ""))
          (if (>= (+ size (length src)) (+ (* 2 size) 1))
              (string-append (string-append prev ".") (print-symbol (take src (- (length src) (- (+ size (length src)) (+ (* 2 size) 1))))))
              (string-append (string-append prev ".") mid))]
         [else
          (cond
            [(>= (+ prev-blank-num (length src)) (+ (* 2 size) 1))
             (string-append (string-append prev ".") (print-symbol (take src (- (length src) (- (+ prev-blank-num (length src)) (+ (* 2 size) 1))))))]
            [(>= (+ prev-blank-num (length src) next-blank-num) (+ (* 2 size) 1))
             (string-append (string-append (string-append (string-append prev ".") mid) ".")
                            (print-blank (- next-blank-num (- (+ prev-blank-num (length src) next-blank-num) (+ (* 2 size) 1)))))]
            [else
             (string-append (string-append (string-append (string-append prev ".") mid) ".") next)])]))]))


;;; Rule tables

(define empty-ruletable ; empty-ruletable: ruletable
  (list 'ruletable))

(define (add-rule rule table) ; add-rule: rule * ruletable -> ruletable
  (append table (list rule)))

(define (make-rule curstate cursym newsym move newstate) ; make-rule: state * symbol * symbol * move * state -> rule
  (list curstate cursym newsym move newstate))

(define (match-rule curstate cursym table) ; match-rule: state * symbol * ruletable -> symbol X move X state
  (define (mapping cur-state cur-sym rule-table)
    (cond
      [(empty? rule-table) null]
      [(and (equal? (first (car rule-table)) cur-state) (equal? (second (car rule-table)) cur-sym))
       (let ([rule (car rule-table)])
         (cons (third rule) (cons (fourth rule) (fifth rule))))]
      [else
       (mapping cur-state cur-sym (cdr rule-table))]))
  (mapping curstate cursym (cdr table))
)


;;; Turing machines

(define (make-tm syms state ruletable) ; make-tm: symbol list * state * ruletable -> tm
  (list (init-tape syms) state ruletable))

(define (step-tm tm) ; step-tm: tm -> tm
  (let ([tape (first tm)] [cur-state (second tm)] [table (third tm)])
    (let ([next-rule (match-rule cur-state (if (equal? (read-tape tape) 'Blank) "-" (read-tape tape)) table)])
      (cond
        [(empty? next-rule) tm]
        [(equal? (cadr next-rule) 'left) (list (move-tape-right (write-tape tape (car next-rule))) (cddr next-rule) table)]
        [(equal? (cadr next-rule) 'right) (list (move-tape-left (write-tape tape (car next-rule))) (cddr next-rule) table)]
        [else
         (list (write-tape tape (car next-rule)) (cddr next-rule) table)]))))

(define (run-tm tm) ; run-tm: tm -> tm
  (let ([tape (first tm)] [cur-state (second tm)] [table (third tm)])
    (let ([next-rule (match-rule cur-state (if (equal? (read-tape tape) 'Blank) "-" (read-tape tape)) table)])
      (if (null? next-rule) tm (run-tm (step-tm tm))))))

; Implement "tm * int -> string" instead of "tm * int -> void".
(define (print-tm tm size) ; print-tm: tm * int -> string
  (print-tape (first tm) size))