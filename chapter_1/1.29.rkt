#lang racket

; From SICP:

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; simpson-integral : (number -> number) number number N -> number
; to compute the integral of f from a to b using Simpson's Rule, 
; where increasing n increases the accuracy
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  
  ; simpson-term : N -> number
  (define (simpson-term k)
    (define coefficient
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 2)
            (else 4)))
    (define y (f (+ a (* k h))))
    (* coefficient y))
  
  (* (/ h 3)
     (sum simpson-term 0 add1 n)))
; Examples: 
(simpson-integral sqr 0 1 1e2)  ; 1/3
(simpson-integral sqr 0 1 1e3)
(simpson-integral cube 0 1 1e2)  ; 1/4
(simpson-integral cube 0 1 1e3)
