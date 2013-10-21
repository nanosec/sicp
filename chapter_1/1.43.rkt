#lang racket

; compose : (Y -> Z) (X -> Y) -> Z
; to produce a function that applies g and then f
(define (compose f g)
  (lambda (x)
    (f (g x))))

; repeated : (X -> X) N[>0] -> (X -> X)
; to produce a function that computes the nth repeated application of f
(define (repeated f n)
  ; repeated-iter : N[>0] (X -> X) -> (X -> X)
  (define (repeated-iter i accumulator)
    (if (= i 1)
        accumulator
        (repeated-iter (- i 1) (compose f accumulator))))
  (repeated-iter n f))
; Examples/Tests: 
(define add12 (repeated add1 12))
(= (add12 2) 14)
(= (add12 5) 17)
(define eighth-power (repeated sqr 3))
(= (eighth-power 2) 256)
(= (eighth-power 3) 6561)