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

(define dx 1e-5)

; smooth : (# -> #) -> (# -> #)
; to produce a function that is the smoothed version of the function f
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
; Examples/Tests: 
(define smoothed-sqr (smooth sqr))
(= (smoothed-sqr 2) (/ (+ (sqr (- 2 dx)) 4 (sqr (+ 2 dx))) 3))
(= (smoothed-sqr 5) (/ (+ (sqr (- 5 dx)) 25 (sqr (+ 5 dx))) 3))
(define smoothed-sin (smooth sin))
(= (smoothed-sin 0) (/ (+ (sin (- dx)) 0 (sin dx)) 3))
(= (smoothed-sin (/ pi 2)) 
   (/ (+ (sin (- (/ pi 2) dx)) 1 (sin (+ (/ pi 2) dx))) 3))

; n-fold-smoothed : (# -> #) N[>0] -> (# -> #)
(define (n-fold-smoothed f n)
  (repeated smooth n))