#lang racket

(provide crazy2add)

(define (return-num i)
  (cond
    ((string=? (symbol->string i) "z") 0)
    ((string=? (symbol->string i) "p") 1)
    ((string=? (symbol->string i) "n") -1)
  )
)

(define (return-char i)
  (cond
    ((= i 3) 'p)
    ((= i 2) 'z)
    ((= i 1) 'p)
    ((= i 0) 'z)
    ((= i -1) 'n)
    ((= i -2) 'z)
    ((= i -3) 'n)
  )
)

(define (carry n1 n2 prev-carry)
  (cond
    ((>= (+ n1 n2 prev-carry) 2) 1)
    ((<= (+ n1 n2 prev-carry) -2) -1)
    (else 0)
  )
)

(define (crazy2add-mid lhs rhs temp-carry lst)
  (cond
    [(and (pair? lhs) (pair? rhs))
     (set! lst (cons lst (return-char (+ (return-num (car lhs)) (return-num (car rhs)) temp-carry))))
     (set! temp-carry (carry (return-num (car lhs)) (return-num (car rhs)) temp-carry))
     (crazy2add-mid (cdr lhs) (cdr rhs) temp-carry lst)
    ]
    [(and (symbol? lhs) (pair? rhs))
     (set! lst (cons lst (return-char (+ (return-num lhs) (return-num (car rhs)) temp-carry))))
     (set! temp-carry (carry (return-num lhs) (return-num (car rhs)) temp-carry))
     (crazy2add-mid 0 (cdr rhs) temp-carry lst)
    ]
    [(and (pair? lhs) (symbol? rhs))
     (set! lst (cons lst (return-char (+ (return-num (car lhs)) (return-num rhs) temp-carry))))
     (set! temp-carry (carry (return-num (car lhs)) (return-num rhs) temp-carry))
     (crazy2add-mid (cdr lhs) 0 temp-carry lst)
    ]
    [(and (number? lhs) (pair? rhs))
     (set! lst (cons lst (return-char (+ (return-num (car rhs)) temp-carry))))
     (set! temp-carry (carry 0 (return-num (car rhs)) temp-carry))
     (crazy2add-mid 0 (cdr rhs) temp-carry lst)
    ]
    [(and (pair? lhs) (number? rhs))
     (set! lst (cons lst (return-char (+ (return-num (car lhs)) temp-carry))))
     (set! temp-carry (carry (return-num (car lhs)) 0 temp-carry))
     (crazy2add-mid (cdr lhs) 0 temp-carry lst)
    ]
    [(and (symbol? lhs) (symbol? rhs))
     (set! lst (cons lst (return-char (+ (return-num lhs) (return-num rhs) temp-carry))))
     (set! temp-carry (carry (return-num lhs) (return-num rhs) temp-carry))
     (crazy2add-mid 0 0 temp-carry lst)
    ]
    [(and (number? lhs) (symbol? rhs))
     (set! lst (cons lst (return-char (+ lhs (return-num rhs) temp-carry))))
     (set! temp-carry (carry 0 (return-num rhs) temp-carry))
     (if (= temp-carry 0) lst (crazy2add-mid 0 0 temp-carry lst))
    ]
    [(and (symbol? lhs) (number? rhs))
     (set! lst (cons lst (return-char (+ rhs (return-num lhs) temp-carry))))
     (set! temp-carry (carry (return-num lhs) 0 temp-carry))
     (if (= temp-carry 0) lst (crazy2add-mid 0 0 temp-carry lst))
    ]
    [(and (number? lhs) (number? rhs))
     (set! lst (cons lst (return-char (+ lhs rhs temp-carry)))) lst
    ]
  )
)

(define (make-result lst)
  (cond
    [(= (length lst) 1) (first lst)]
    [(= (length lst) 2) (cons (first lst) (last lst))]
    [else (cons (first lst) (make-result (rest lst)))]
  )
)

(define (crazy2add lhs rhs)
  (define temp-carry 0)
  (define lst null)
  (set! lst (flatten (crazy2add-mid lhs rhs temp-carry lst)))
  (make-result lst)
)