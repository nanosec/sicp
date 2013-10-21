#lang racket

; compose : (Y -> Z) (X -> Y) -> Z
; to produce a function that applies g and then f
(define (compose f g)
  (lambda (x)
    (f (g x))))
; Examples/Tests: 
(define sqr-add1 (compose sqr add1))
(= (sqr-add1 6) 49)
(= (sqr-add1 14) 225)
(define add2-mult3 (compose (lambda (x) (+ x 2))
                            (lambda (x) (* 3 x))))
(= (add2-mult3 5) 17)
(= (add2-mult3 10) 32)