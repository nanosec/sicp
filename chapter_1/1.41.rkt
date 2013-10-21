#lang racket

; double : (X -> X) -> (X -> X)
; to return a function that applies the function f twice
(define (double f)
  (lambda (x)
    (f (f x))))
; Examples/Tests:
(define add2 (double add1))
(= (add2 0) 2)
(= (add2 5) 7)
(define *100 (double (lambda (x) (* 10 x))))
(= (*100 3) 300)
(= (*100 -4) -400)
(define add16 ((double (double double)) add1))
(= (add16 1) 17)
(= (add16 5) 21)