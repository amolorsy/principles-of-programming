#lang racket

; We auto-grade only vlencode function; other functions are not
; auto-graded.
; If this "provide" statement is omitted, your submission will be graded 0.
(provide vlencode)

(define (get-lowest-frequency list)
  (cond
    [(= (length list) 1) (car list)]
    [else (let ([rest (get-lowest-frequency (cdr list))])
            (if (> (get-val (car list)) (get-val rest)) rest (car list)))]
  )
)

(define (get-val tree) ; get-val: tree -> value
  (if (isleaf? tree) (leafval tree) (nodeval tree))
)

(define (make-tree frequencies) ; make-tree: (string X int) list -> tree
  (cond
    [(= (length frequencies) 1) (list (cons (leafstr (car frequencies)) (list 0)))]
    [(= (length frequencies) 2) (let ([left (first frequencies)] [right (last frequencies)])
                                  (if (< (get-val left) (get-val right)) (node (+ (get-val left) (get-val right)) left right)
                                      (node (+ (get-val right) (get-val left)) right left)))] 
    [else (let ([left (get-lowest-frequency frequencies)])
            (let ([right (get-lowest-frequency (remove left frequencies))])
              (make-tree (append (remove left (remove right frequencies)) (list (node (+ (get-val left) (get-val right)) left right))))))]
  )
)

(define (make-code tree obj code)
  (cond
    [(isleaf? tree) (if (equal? tree obj) code null)]
    [else (append (make-code (leftsub tree) obj (append code (list 0))) (make-code (rightsub tree) obj (append code (list 1))))]
  )
)

(define (delete-zero frequencies lst) 
  (cond
    [(= (length frequencies) 1) (if (equal? (get-val (first frequencies)) 0) lst (append lst frequencies))]
    [else (if (equal? (get-val (first frequencies)) 0) (delete-zero (rest frequencies) lst)
              (delete-zero (rest frequencies) (append lst (list (first frequencies)))))] 
  )
)

(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  (cond
    [(= (length frequencies) 1) (list (cons (leafstr (first frequencies)) (list 0)))]
    [else
     (define zero-deleted (delete-zero frequencies null))
     (define (make-result lst)
       (cond
         [(= (length lst) 1) (list (cons (leafstr (first lst)) (make-code (make-tree zero-deleted) (first lst) null)))]
         [else (append (list (cons (leafstr (first lst)) (make-code (make-tree zero-deleted) (first lst) null)))
                       (make-result (rest lst)))]))
     (make-result zero-deleted)])
)

; (vlencode (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1)))
; (vlencode (list (cons "a" 16) (cons "d" 17) (cons "f" 25) (cons "c" 12) (cons "b" 5) (cons "e" 10)))

;   The output of vlencode should follow the following form.
;   The exact code for each word can be different from this example,
;   but the length of the code for each word should be the same.
;  
;   (define frequencies (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0)) (cons "b" (list 1 0)) (cons "c" (list 1 1 0)) (cons "d" (list 1 1 1)))
;
;   (define frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))
;
;   (define frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6) (cons "e" 0)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))

; You may need the following tree interface (but not mandatory.)

(define (leaf str val) ; leaf: string * int -> tree
  (cons str val)
)

(define (node val lsub rsub) ; node: tree * tree * int -> tree
  (list val (list lsub rsub))
)

(define (isleaf? tree) ; isleaf?: tree -> bool
  (not (list? tree)) 
)

(define (leafstr tree) ; leftstr: tree -> string
  (car tree)
)

(define (leafval tree) ; leafval: tree -> int
  (cdr tree)
)

(define (nodeval tree) ; rootval: tree -> value
  (first tree)
)

(define (leftsub tree) ; leftsub: tree -> tree
  (first (last tree))
)

(define (rightsub tree) ; rightsub: tree -> tree
  (last (last tree))
)