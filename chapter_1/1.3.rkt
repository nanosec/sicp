#lang racket

; sum-squares-of-two-largest : number number number -> number
; to sum the squares of the two larger of three numbers
(define (sum-squares-of-two-largest number-1 number-2 number-3)
  ; sum-squares : number number -> number
  ; to sum the squares of number-1 and number-2
  (define (sum-squares number-1 number-2)
    (+ (sqr number-1) (sqr number-2)))
  (cond 
    [(<= number-1 number-2 number-3) (sum-squares number-2 number-3)]
    [(>= number-1 number-2 number-3) (sum-squares number-1 number-2)]
    [else (sum-squares number-1 number-3)]))
; Examples/Tests: 
(= (sum-squares-of-two-largest -3 0 2) 4)
(= (sum-squares-of-two-largest 2 2 2) 8)
(= (sum-squares-of-two-largest 4 -5 3) 25)
(= (sum-squares-of-two-largest 2 1 0) 5)
