#lang racket

(provide yanghui)

(define (pascal row col)
  (cond
    [(= col 0) 1]
    [(= col row) 1]
    [else (/ (* (- row (- col 1)) (pascal row (- col 1))) col)]
  )
)

(define (yanghui-floor floor n)
  (cond
    [(= floor 1) "1"]
    [(= n 1) "1"]
    [else (string-append (number->string (pascal (- floor 1) (- n 1))) (yanghui-floor floor (- n 1)))]
  )
)

(define (make-full-string n)
  (define full-str "")
  (for ([i n]) (set! full-str (string-append full-str (yanghui-floor (+ i 1) (+ i 1)))))
  full-str
)

(define (yanghui n)
  (if (= n 0) "" (make-full-string (abs n)))
)