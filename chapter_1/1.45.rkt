#lang racket

(define tolerance 1e-5)

; fixed-point : (# -> #) # -> #
(define (fixed-point f first-guess)
  ; try : # -> #
  (define (try guess)
    ; close-enough? : # # -> boolean
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; average-damp : (# -> #) -> (# -> #)
(define (average-damp f)
  (lambda (x) 
    (/ (+ x (f x)) 2)))

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

; nth-root : # N -> #
; to compute the nth root of x
(define (nth-root x n)
  ; non-damped-mapping : # -> #
  (define (non-damped-mapping y)
    (/ x (expt y (- n 1))))
  ; average-damp-repeated : (# -> #) -> (# -> #)
  (define average-damp-repeated 
    (let ((number-of-damps (floor (/ (log n) (log 2)))))
      (repeated average-damp number-of-damps)))
  (fixed-point (average-damp-repeated non-damped-mapping) 1.0))

; Examples/Tests: 

; within-tolerance? : # # -> boolean
(define (within-tolerance? test-expression expected-value)
  (< (abs (- test-expression expected-value)) tolerance))

(within-tolerance? (nth-root 25 2) 5)
(within-tolerance? (nth-root 1331 3) 11)
(within-tolerance? (nth-root 2401 4) 7)
(within-tolerance? (nth-root 32768 5) 8)
(within-tolerance? (nth-root 60466176 10) 6)
(within-tolerance? (nth-root 14348907 15) 3)