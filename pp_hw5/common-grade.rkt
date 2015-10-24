#lang racket

(provide output output-raw)

(define (output p)
  (with-handlers
   ([(lambda (exn) #t) (lambda (exn) (printf "X (error)\n"))])
   (printf (if (equal? (p) #t) "O\n" "X\n"))))

(define (output-raw p)
  (printf (if (equal? (p) #t) "O\n" "X\n")))
