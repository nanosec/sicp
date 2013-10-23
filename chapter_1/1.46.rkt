#lang racket

; iterative-improve : (X -> boolean) (X -> X) -> (X -> X)
(define (iterative-improve good-enough? improve-guess)
  ; improve-until-good-enough : X -> X
  (define (improve-until-good-enough guess)
    (let ((next-guess (improve-guess guess)))
      (if (good-enough? next-guess)
          next-guess
          (improve-until-good-enough (improve-guess next-guess)))))
  improve-until-good-enough)

; Examples/Tests: 

(define tolerance 1e-5)

; within-tolerance? : # # -> boolean
(define (within-tolerance? test-expression expected-value)
  (< (abs (- test-expression expected-value)) tolerance))

; mean : # # -> #
(define (mean x y)
  (/ (+ x y) 2))

; square-root : # -> #
(define (square-root x)
  ; good-enough? : # -> boolean
  (define (good-enough? guess)
    (within-tolerance? (sqr guess) x))
  ; improve-guess : # -> #
  (define (improve-guess guess)
    (mean guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))
; Examples/Tests: 
(within-tolerance? (square-root 25) 5)
(within-tolerance? (square-root 13689) 117)

; fixed-point : (# -> #) # -> #
(define (fixed-point f first-guess)
  ; good-enough? : # -> boolean
  (define (good-enough? guess)
    (within-tolerance? (improve-guess guess) guess))
  ; improve-guess : # -> #
  (define (improve-guess guess)
    (mean guess (f guess)))
  ((iterative-improve good-enough? improve-guess) first-guess))
; Examples/Tests: 
(within-tolerance? (fixed-point cos 1) 0.739085)
(within-tolerance? (fixed-point (lambda (x) (/ 1 (sin x))) 5) 1.114157)