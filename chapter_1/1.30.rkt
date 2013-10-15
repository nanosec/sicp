#lang racket

; From SICP:

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; sum-iter : (# -> #) # (# -> #) # -> #
(define (sum-iter term a0 next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a0 0))
; Examples/Tests: 
(define (sum-squares a b) (sum-iter sqr a add1 b))
(= (sum-squares 1 5) 55)
(= (sum-squares -1.5 2.5) 11.25)
(define (sum-cubes a b) (sum-iter cube a add1 b))
(= (sum-cubes 1 5) 225)
(= (sum-cubes -3 5) 189)
