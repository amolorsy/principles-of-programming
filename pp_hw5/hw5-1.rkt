#lang racket


;;; CAUTION: read VERY CAREFULLY hw5-1-grade.rkt before doing your HW.
;;; Instructions how to write submission (and grade it) is written in
;;; hw5-1-grade.rkt.

;;; If these statements are omitted, your submission will be graded 0.
;;; You can add whatever function you would like to make public.
;;; For example, if you want a function foo in hw5-2, provide it.

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)


;;; primitive tile
; A black tile is 'B and a whit etile is 'W.

(define black ; black: form
  'B)
(define white ; white: form
  'W)


;;; complex tile
;
; An array tile looks like:
; (cons 'array (list row_1 row_2 ... row_n)),
; for each row_i = (list cell_i1 ... cell_in).
;
; Examples:
; 1.
; (cons 'array (list (list 'B 'B) (list 'W 'W)))
; BB
; WW
;
; 2.
; (cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B)))
; BBBB
; BBBB
; WWBB
; WWBB
;
;
; An tree tile looks like:
; (cons 'tree (list subtree_nw subtree_ne subtree_se subtree_sw)).
;
; Examples:
; 1.
; (cons 'tree (list 'B 'B 'W 'B))
; BB
; BW
;
; 2.
; (cons 'tree (list (list 'B 'B 'B 'W) (list 'B 'B 'W 'B) (list 'B 'W 'B 'B) (list 'W 'B 'B 'B)))
; BBBB
; WBBW
; WBBW
; BBBB
;
; See hw5-1-grade.rkt for more details on grading array and tree representation.

(define (glue-array-from-tree nw ne se sw) ; glue-array-from-tree: form * form * form * form -> form
  (glue-array-from-array (tree-to-array nw)
                         (tree-to-array ne)
                         (tree-to-array se)
                         (tree-to-array sw))
)

(define (glue-array-from-array nw ne se sw) ; glue-array-from-array: form * form * form * form -> form
  (cond
    [(or (equal? nw 'B) (equal? nw 'W)) (cons 'array (list (list nw ne) (list sw se)))]
    [else
     (define (making-list src1 src2)
       (define lst '())
       (for ([i (length src1)]) (set! lst (append lst (list (flatten (list (list-ref src1 i) (list-ref src2 i)))))))
       lst)
     (cons 'array (append (making-list (cdr nw) (cdr ne)) (making-list (cdr sw) (cdr se))))])
)

(define (glue-tree-from-tree nw ne se sw) ; glue-tree-from-tree: form * form * form * form -> form
  (cond
    [(or (equal? nw 'B) (equal? nw 'W)) (cons 'tree (list nw ne se sw))]
    [else
     (cons 'tree (let ([src-nw (cdr nw)] [src-ne (cdr ne)] [src-se (cdr se)] [src-sw (cdr sw)])
                   (list
                    (list (first src-nw) (second src-nw) (third src-nw) (fourth src-nw))
                    (list (first src-ne) (second src-ne) (third src-ne) (fourth src-ne))
                    (list (first src-se) (second src-se) (third src-se) (fourth src-se))
                    (list (first src-sw) (second src-sw) (third src-sw) (fourth src-sw)))))])
)

(define (glue-tree-from-array nw ne se sw) ; glue-tree-from-array: form * form * form * form -> form
  (glue-tree-from-tree (array-to-tree nw)
                       (array-to-tree ne)
                       (array-to-tree se)
                       (array-to-tree sw))
)

(define (rotate-array f) ; rotate-array: form -> form
  (cond
    [(equal? f 'B) 'B]
    [(equal? f 'W) 'W]
    [(= (length (first (cdr f))) 2)
     (cons 'array (list (list (first (last (cdr f))) (first (first (cdr f)))) (list (last (last (cdr f))) (last (first (cdr f))))))]
    [else
     (define row-num (length (cdr f)))
     (glue-array-from-array (rotate-array (cons 'array (map (lambda (lst-sw) (take lst-sw (/ (length lst-sw) 2))) (take-right (cdr f) (/ row-num 2)))))
                            (rotate-array (cons 'array (map (lambda (lst-nw) (take lst-nw (/ (length lst-nw) 2))) (take (cdr f) (/ row-num 2)))))
                            (rotate-array (cons 'array (map (lambda (lst-ne) (take-right lst-ne (/ (length lst-ne) 2))) (take (cdr f) (/ row-num 2)))))
                            (rotate-array (cons 'array (map (lambda (lst-se) (take-right lst-se (/ (length lst-se) 2))) (take-right (cdr f) (/ row-num 2))))))]
  )
)

(define (neighbor-array location f) ; neighbor-array: location * form -> int
  (cond
    [(not (list? f)) 0]
    [else
     (define axis-size (length (cdr f)))
     (define (get-position loc tile pos power)
       (cond
         [(empty? loc) pos]
         [else
          (let ([x (car loc)] [row-num (length (cdr tile))])
            (cond
              [(= x 0) (get-position (cdr loc)
                                       (cons 'array (map (lambda (lst-nw) (take lst-nw (/ (length lst-nw) 2))) (take (cdr tile) (/ row-num 2))))
                                       (cons (- (car pos) power) (- (cdr pos) power))
                                       (/ power 2))]
              [(= x 1) (get-position (cdr loc)
                                       (cons 'array (map (lambda (lst-ne) (take-right lst-ne (/ (length lst-ne) 2))) (take (cdr tile) (/ row-num 2))))
                                       (cons (- (car pos) power) (cdr pos))
                                       (/ power 2))]
              [(= x 2) (get-position (cdr loc)
                                       (cons 'array (map (lambda (lst-se) (take-right lst-se (/ (length lst-se) 2))) (take-right (cdr tile) (/ row-num 2))))
                                       (cons (car pos) (cdr pos))
                                       (/ power 2))]
              [(= x 3) (get-position (cdr loc)
                                       (cons 'array (map (lambda (lst-sw) (take lst-sw (/ (length lst-sw) 2))) (take-right (cdr tile) (/ row-num 2))))
                                       (cons (car pos) (- (cdr pos) power))
                                       (/ power 2))]))]))
     (let ([pos (get-position location f (cons axis-size axis-size) (/ axis-size 2))])
       (let ([xpos (car pos)] [ypos (cdr pos)])
         (define (calculate lst)
           (cond
             [(empty? lst) 0]
             [else
              (if (equal? (list-ref (list-ref (cdr f) (- (car (car lst)) 1)) (- (cdr (car lst)) 1)) 'B)
                  (+ 1 (calculate (cdr lst))) (calculate (cdr lst)))]))
         (cond
           [(and (equal? xpos 1) (equal? ypos 1))
            (calculate (list (cons xpos (+ ypos 1)) (cons (+ xpos 1) ypos) (cons (+ xpos 1) (+ ypos 1))))]
           [(and (equal? xpos 1) (equal? ypos axis-size))
            (calculate (list (cons xpos (- ypos 1)) (cons (+ xpos 1) (- ypos 1)) (cons (+ xpos 1) ypos)))]
           [(and (equal? xpos axis-size) (equal? ypos 1))
            (calculate (list (cons (- xpos 1) ypos) (cons (- xpos 1) (+ ypos 1)) (cons xpos (+ ypos 1))))]
           [(and (equal? xpos axis-size) (equal? ypos axis-size))
            (calculate (list (cons (- xpos 1) (- ypos 1)) (cons (- xpos 1) ypos) (cons xpos (- ypos 1))))]
           [(and (equal? xpos 1) (> ypos 1) (< ypos axis-size))
            (calculate (list (cons xpos (- ypos 1)) (cons xpos (+ ypos 1)) (cons (+ xpos 1) (- ypos 1)) (cons (+ xpos 1) ypos) (cons (+ xpos 1) (+ ypos 1))))]
           [(and (equal? ypos axis-size) (> xpos 1) (< xpos axis-size))
            (calculate (list (cons (- xpos 1) (- ypos 1)) (cons (- xpos 1) ypos) (cons xpos (- ypos 1)) (cons (+ xpos 1) (- ypos 1)) (cons (+ xpos 1) ypos)))]
           [(and (equal? xpos axis-size) (> ypos 1) (< ypos axis-size))
            (calculate (list (cons (- xpos 1) (- ypos 1)) (cons (- xpos 1) ypos) (cons (- xpos 1) (+ ypos 1)) (cons xpos (- ypos 1)) (cons xpos (+ ypos 1))))]
           [(and (equal? ypos 1) (> xpos 1) (< xpos axis-size))
            (calculate (list (cons (- xpos 1) ypos) (cons (- xpos 1) (+ ypos 1)) (cons xpos (+ ypos 1)) (cons (+ xpos 1) ypos) (cons (+ xpos 1) (+ ypos 1))))]
           [else
            (calculate (list (cons (- xpos 1) (- ypos 1)) (cons (- xpos 1) ypos) (cons (- xpos 1) (+ ypos 1))
                  (cons xpos (- ypos 1)) (cons xpos (+ ypos 1))
                  (cons (+ xpos 1) (- ypos 1)) (cons (+ xpos 1) ypos) (cons (+ xpos 1) (+ ypos 1))))])))]))
                 

; In the document, it is said to have type form -> void, but implement
; as form -> string.
; Read hw5-1-grade.rkt for formatting.

(define (pprint-array f) ; pprint-array: form -> string
  (cond
    [(equal? f 'B) "B\n"]
    [(equal? f 'W) "W\n"]
    [else
     (define (print-row lst)
       (cond
         [(empty? lst) "\n"]
         [else
          (string-append (symbol->string (car lst)) (print-row (cdr lst)))]))
     (define (print-all lst)
       (cond
         [(empty? lst) ""]
         [else (string-append (print-row (car lst)) (print-all (cdr lst)))]))
     (print-all (cdr f))])
)

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'array (car f)) #t]
        [else #f]))


;;; implementation with tree

(define (rotate-tree f) ; rotate-tree: form -> form
  (cond
    [(equal? f 'B) 'B]
    [(equal? f 'W) 'W]
    [(not (list? (first (cdr f)))) (cons 'tree (list (fourth (cdr f)) (first (cdr f)) (second (cdr f)) (third (cdr f))))]
    [else
     (glue-tree-from-tree (rotate-tree (cons 'tree (fourth (cdr f))))
                          (rotate-tree (cons 'tree (first (cdr f))))
                          (rotate-tree (cons 'tree (second (cdr f))))
                          (rotate-tree (cons 'tree (third (cdr f)))))]
  )
)

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  (neighbor-array loc (tree-to-array f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint-tree f) ; pprint-tree: form -> string
  (pprint-array (tree-to-array f)))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'tree (car f)) #t]
        [else #f]))


;;; conversions 

(define (array-to-tree f) ; array-to-tree: form -> form
  (cond
    [(equal? f 'B) 'B]
    [(equal? f 'W) 'W]
    [(equal? (length (first (cdr f))) 2)
     (cons 'tree (append (first (cdr f)) (reverse (second (cdr f)))))]
    [(equal? (length (first (cdr f))) 4)
     (let ([src1 (first (cdr f))] [src2 (second (cdr f))] [src3 (third (cdr f))] [src4 (fourth (cdr f))])
       (cons 'tree (list (append (take src1 2) (reverse (take src2 2)))
                         (append (take-right src1 2) (reverse (take-right src2 2)))
                         (append (take-right src3 2) (reverse (take-right src4 2)))
                         (append (take src3 2) (reverse (take src4 2))))))]
    [else
     (define row-num (length (cdr f)))
     (glue-tree-from-tree (array-to-tree (cons 'array (map (lambda (lst-nw) (take lst-nw (/ (length lst-nw) 2))) (take (cdr f) (/ row-num 2))))) 
                          (array-to-tree (cons 'array (map (lambda (lst-ne) (take-right lst-ne (/ (length lst-ne) 2))) (take (cdr f) (/ row-num 2))))) 
                          (array-to-tree (cons 'array (map (lambda (lst-se) (take-right lst-se (/ (length lst-se) 2))) (take-right (cdr f) (/ row-num 2))))) 
                          (array-to-tree (cons 'array (map (lambda (lst-sw) (take lst-sw (/ (length lst-sw) 2))) (take-right (cdr f) (/ row-num 2))))))])
)

(define (tree-to-array f) ; tree-to-array: form -> form
  (cond
    [(equal? f 'B) 'B]
    [(equal? f 'W) 'W]
    [(or (equal? (first (cdr f)) 'B) (equal? (first (cdr f)) 'W))
     (cons 'array (list (list (first (cdr f)) (second (cdr f))) (list (fourth (cdr f)) (third (cdr f)))))]
    [(or (equal? (first (first (cdr f))) 'B) (equal? (first (first (cdr f))) 'W))
     (let ([nw (first (cdr f))] [ne (second (cdr f))] [se (third (cdr f))] [sw (fourth (cdr f))])
       (cons 'array (list
                     (append (take nw 2) (take ne 2))
                     (append (reverse (take-right nw 2)) (reverse (take-right ne 2)))
                     (append (take sw 2) (take se 2))
                     (append (reverse (take-right sw 2)) (reverse (take-right se 2))))))]
    [else
     (glue-array-from-array (tree-to-array (cons 'tree (first (cdr f))))
                            (tree-to-array (cons 'tree (second (cdr f))))
                            (tree-to-array (cons 'tree (third (cdr f))))
                            (tree-to-array (cons 'tree (fourth (cdr f)))))]
  )
)

;;; interfaces

(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  (if (is-array? nw)
      (glue-array-from-array nw ne se sw)
      (glue-tree-from-tree nw ne se sw)))

(define (rotate f) ; rotate: form -> form
  (if (is-array? f)
      (rotate-array f)
      (rotate-tree f)))

(define (neighbor loc f) ; neighbor: location * form -> int
  (if (is-array? f)
      (neighbor-array loc f)
      (neighbor-tree loc f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint f) ; pprint: form -> string
  (if (is-array? f)
      (pprint-array f)
      (pprint-tree f)))