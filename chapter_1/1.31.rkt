#lang racket

; product : (# -> #) # (# -> #) # -> #
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; product-iter : (# -> #) # (# -> #) # -> #
(define (product-iter term a0 next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a0 1))
; Examples/Tests: 
(define (factorial n) (product identity 2 add1 n))
(define (factorial-iter n) (product-iter identity 2 add1 n))
(and (= (factorial 5) 120) (= (factorial-iter 5) 120))
(and (= (factorial 8) 40320) (= (factorial-iter 8) 40320))

; pi/4 : N -> number
; to compute an approximation of pi/4 using n+1 terms
(define (pi/4 n)
  ; pi/4-term : N -> number
  (define (pi/4-term k)
    (define numerator (* 2 (+ (ceiling (/ k 2)) 1)))
    (define denominator (+ (* 2 (floor (/ k 2))) 3))
    (/ numerator denominator))
  (product pi/4-term 0 add1 n))
; Examples/Tests: 
(= (pi/4 0) (/ 2 3))
(= (pi/4 2) (/ (* 2 4 4) (* 3 3 5)))
(- (pi/4 100) (/ pi 4))
(- (pi/4 1000) (/ pi 4))
