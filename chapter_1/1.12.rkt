#lang racket

; pascal : N N -> N
; to compute the jth element of the ith row of Pascal's triangle
(define (pascal i j)
  (cond 
    [(or (= j 0) (= j i)) 1]
    [else (+ (pascal (- i 1) (- j 1))
             (pascal (- i 1) j))]))
; Examples/Tests: 
(= (pascal 0 0) 1)
(= (pascal 3 1) 3)
(= (pascal 4 2) 6)
(= (pascal 5 4) 5)
(= (pascal 6 3) 20)
