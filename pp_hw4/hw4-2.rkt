#lang racket

; We auto-grade only execute function; other functions are not
; auto-graded. However, S, K, I, v, and a are required for
; grading. See hw4-2-grade.rkt for more information.
(provide execute S K I v a)

; Implement execute. 
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".

(define (execute e) ; execute: E -> string
  (define (make-ski e)
    (cond
      [(not (isa? e)) e]
      [(and (isa? e) (isI? (al e))) (make-ski (ar e))]
      [(and (isa? e) (not (isa? (al e))) (not (isa? (ar e)))) e]
      [(and (isa? e) (isa? (al e)) (isK? (al (al e)))) (make-ski (ar (al e)))]
      [(and (isa? e) (isa? (al e)) (isa? (al (al e))) (isS? (al (al (al e))))) (make-ski (a (a (ar (al (al e))) (ar e)) (a (ar (al e)) (ar e))))]
      [else (let ([temp (a (make-ski (al e)) (make-ski (ar e)))])
              (if (equal? temp e) temp (make-ski temp)))]
    )
  )
  (pprint (make-ski e))
)

(define S ; S: E
  'S
)
(define K ; K: E
  'K
)
(define I ; I: E
  'I
)
(define (v str) ; v: string -> E
  (string->symbol str)
)
(define (a lhs rhs) ; a: E * E -> E
  (list lhs rhs)
)

; You may need the following tree interface.

(define (isS? e) ; isS?: E -> bool
  (equal? e S)
)
(define (isK? e) ; isK?: E -> bool
  (equal? e K)
)
(define (isI? e) ; isI?: E -> bool
  (equal? e I)
)
(define (isv? e) ; isv?: E -> bool
  (and (symbol? e) (not (isS? e)) (not (isK? e)) (not (isI? e)))
)
(define (isa? e) ; isa?: E -> bool
  (and (list? e) (= (length e) 2))
)
(define (var e) ; var: E -> string
  (symbol->string e)
)
(define (al e) ; al: E -> E
  (first e)
)
(define (ar e) ; ar: E -> E
  (last e)
)
(define (pprint e) ; pprint: E -> string
  (cond
    [(isv? e) (var e)]
    [(and (isa? e) (not (isa? (al e))) (not (isa? (ar e)))) (string-append (string-append "(" (string-join (map symbol->string e) " ")) ")")]
    [else (string-append (string-append (string-append (string-append "(" (pprint (al e))) " ") (pprint (ar e))) ")")])
)