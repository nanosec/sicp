#lang racket

; square-root : number[> 0] -> number
; to compute the square root of a-number (> 0) using Newton's method
(define (square-root a-number)
  ; sqrt-iter : number -> number
  (define (sqrt-iter guess)
    ; average : number number -> number
    (define (average x y)
      (/ (+ x y) 2))
    
    (define improved-guess
      (average guess (/ a-number guess)))
    
    (define tolerance 0.001)
    
    (define good-enough?-old
      (< (abs (- (sqr guess) a-number))
         tolerance))
    
    (define good-enough?
      (< (abs (- 1 (/ improved-guess guess)))
         tolerance))
    
    (if good-enough?
        improved-guess
        (sqrt-iter improved-guess)))
  (sqrt-iter 1.0))
; Tests: 
;(square-root 0)  ; good-enough?-old only
(square-root 1)
(square-root 2)
(square-root (sqr 1e-4))
(square-root (sqr 1e40))  ; good-enough? only
