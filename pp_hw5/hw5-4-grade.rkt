#lang racket

(require "common-grade.rkt")
(require "hw5-4.rkt")

(define (item-match? l r)
  (and (equal? (car l) (car r))
       (or
        (and (< (cdr l) 0.001) (< (cdr r) 0.001))
        (let ([ratio (/ (cdr l) (cdr r))])
         (and (< 0.99 ratio) (< ratio 1.01))))))

(define (list-match? pred l r)
  (cond [(equal? l '()) (equal? r '())]
        [(equal? r '()) #f]
        [else (and (pred (car l) (car r)) (list-match? pred (cdr l) (cdr r)))]))

(define (item-nonzero? sv)
  (>= (cdr sv) 0.001))

(define (symbol<? l r)
  (string<? (symbol->string l) (symbol->string r)))

(define (match? l r)
  (let* ([ll (filter item-nonzero? l)]
         [lll (sort ll (lambda (l r) (symbol<? (car l) (car r))))]
         [rr (filter item-nonzero? r)]
         [rrr (sort rr (lambda (l r) (symbol<? (car l) (car r))))])
    (list-match? item-match? lll rrr)))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D D 1.0))]
          [model-output (list (cons 'A 15.0) (cons 'B 25.0) (cons 'C 7.5) (cons 'D 52.5))]
          [output (catchYou input 1)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D D 1.0))]
          [model-output (list (cons 'A 4.5) (cons 'B 15.0) (cons 'C 7.5) (cons 'D 73.0))]
          [output (catchYou input 2)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D C 0.3) (D D 0.7))]
          [model-output '((A . 12.162162161758499) (B . 12.162162161758502) (C . 20.2702702695975) (D . 55.4054054035665))]
          [output (catchYou input 100000000000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B A 1.0) (C A 0.3) (C C 0.7))]
          [model-output '((A . 47.05882352941176) (B . 52.94117647058823) (C . 0.00000000000001))]
          [output (catchYou input 100000000000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D A 0.2) (D D 0.8) (E B 0.4) (E E 0.6))]
          [model-output '((A . 15.702111674569753) (B . 15.702111674569753) (C . 4.7106335023709285) (D . 64.37865786573602) (E . 0.00000000000001))]
          [output (catchYou input 100000000000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))



(output
 (lambda ()
   (let* ([input '((A B 1.0) (B C 0.3) (B D 0.7) (C A 0.6) (C D 0.4) (D C 0.3) (D D 0.7))]
          [model-output '((A . 12.103341361604489) (B . 12.103341361604489) (C . 20.17223560267416) (D . 55.1374439806426))]
          [output (catchYou input 100000000000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A B 0.34) (A C 0.66) (B C 0.3) (B E 0.7) (C A 0.23) (C D 0.4) (C E 0.37) (D A 0.2) (D C 0.5) (D D 0.3) (E B 0.4) (E E 0.6))]
          [model-output '((A . 4.695349496397151) (B . 22.141527763508222) (C . 13.637944595344415) (D . 7.793111197339664) (E . 51.362772336832954))]
          [output (catchYou input 100000000000000)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))

(output
 (lambda ()
   (let* ([input '((A A 0.3) (A B 0.1) (A C 0.3) (A D 0.2) (A E 0.1) (B A 0.2) (B B 0.2) (B C 0.4) (B D 0.1) (B E 0.1) (C A 0.1) (C B 0.2) (C C 0.1) (C D 0.3) (C E 0.3) (D A 0.5) (D B 0.1) (D C 0.2) (D D 0.1) (D E 0.1) (E A 0.1) (E B 0.3) (E C 0.2) (E D 0.2) (E E 0.2))]
          [model-output '((A . 24.040684237773945) (B . 17.355524736269885) (C . 23.522884884960355) (D . 18.74248728844915) (E . 16.338418864671755))]
          [output (catchYou input 87654321)])
     (printf "~s~n" model-output)
     (printf "~s~n" output)
     (match? model-output output))))