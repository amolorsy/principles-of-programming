#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw4-2.rkt")

(output
 (lambda ()
   (equal?
    (execute (a (a (a S K) I) (v "x")))
    "x")))
(output
 (lambda ()
   (equal?
    (execute (a (a K (v "x")) (a I (v "x"))))
    "x")))
(output
 (lambda ()
   (equal?
    (execute (a (a (a (v "x") (v "y")) (v "z")) (v "w")))
    "(((x y) z) w)")))
