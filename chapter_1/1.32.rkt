#lang racket

; accumulate : (# # -> #) # (# -> #) # (# -> #) # -> #
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; accumulate-iter : (# # -> #) # (# -> #) # (# -> #) # -> #
(define (accumulate-iter combiner null-value term a0 next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a0 null-value))

; Examples/Tests: 

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (sum-squares a b) (sum sqr a add1 b))
(= (sum-squares 1 5) 55)
(= (sum-squares -1.5 2.5) 11.25)
(define (cube x) (* x x x))
(define (sum-cubes a b) (sum-iter cube a add1 b))
(= (sum-cubes 1 5) 225)
(= (sum-cubes -3 5) 189)

(define (product term a next b)
  (accumulate * 1 term a next b))
(define (product-iter term a next b)
  (accumulate * 1 term a next b))

(define (factorial n) (product identity 2 add1 n))
(define (factorial-iter n) (product-iter identity 2 add1 n))
(and (= (factorial 5) 120) (= (factorial-iter 5) 120))
(and (= (factorial 8) 40320) (= (factorial-iter 8) 40320))
