#lang racket

; cube-root : number[>= 0] -> number
; to compute the cube root of a-number (>= 0) using Newton's method
(define (cube-root a-number)
  ; cube-root-iter : number -> number
  (define (cube-root-iter guess)
    (define improved-guess
      (/ (+ (/ a-number (sqr guess))
            (* 2 guess))
         3))
    
    (define tolerance 0.001)
    
    (define good-enough?-old
      (< (abs (- (expt guess 3) a-number))
         tolerance))
    
    (define good-enough?
      (< (abs (- 1 (/ improved-guess guess)))
         tolerance))
    
    (if good-enough?
        improved-guess
        (cube-root-iter improved-guess)))
  (cube-root-iter 1.0))
; Tests: 
(cube-root 0)
(cube-root 1)
(cube-root 2)
(cube-root (expt 1e-4 3))
(cube-root (expt 1e40 3))