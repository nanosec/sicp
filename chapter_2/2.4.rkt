#lang racket

; cons : X Y -> ((X Y -> W) -> W)
(define (cons x y)
  (lambda (m) (m x y)))

; car : ((X Y -> W) -> W) -> X
(define (car z)
  (z (lambda (p q) p)))

; cdr : ((X Y -> W) -> W) -> Y
(define (cdr z)
  (z (lambda (p q) q)))

; Examples/Tests: 
(= (car (cons 1 2)) 1)
(= (cdr (cons 1 2)) 2)
(= (car (cons 3 4)) 3)
(= (cdr (cons 3 4)) 4)