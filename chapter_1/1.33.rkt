#lang racket

; filtered-accumulate : (# -> bool) (# # -> #) # (# -> #) # (# -> #) # -> #
(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filtered-accumulate predicate 
                                         combiner 
                                         null-value 
                                         term 
                                         (next a) 
                                         next 
                                         b))
          (filtered-accumulate predicate 
                               combiner 
                               null-value 
                               term 
                               (next a) 
                               next 
                               b))))

; filtered-accumulate-iter : (# -> bool) (# # -> #) # (# -> #) # (# -> #) # -> #
(define (filtered-accumulate-iter predicate combiner null-value term a0 next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a0 null-value))

; Examples/Tests: 

; prime? : N -> boolean
; to determine if n is prime
(define (prime? n)
  ; find-divisor : N N -> N
  (define (find-divisor n test-divisor)
    ; divides? : N N -> boolean
    (define (divides? a b)
      (= (remainder b a) 0))
    (cond ((> (sqr test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define smallest-divisor (find-divisor n 2))
  (cond ((= n 1) #f)
        (else (= n smallest-divisor))))

; sum-primes-squared : N N -> N
; to sum the squares of the primes in the interval [a,b]
(define (sum-primes-squared a b)
  (filtered-accumulate prime? + 0 sqr a add1 b))

(define (sum-primes-squared-iter a b)
  (filtered-accumulate-iter prime? + 0 sqr a add1 b))

; Examples/Tests: 
(and (= (sum-primes-squared 0 11) 208) 
     (= (sum-primes-squared-iter 0 11) 208))
(and (= (sum-primes-squared 5 18) 653) 
     (= (sum-primes-squared-iter 5 18) 653))
(and (= (sum-primes-squared 50 60) 6290)
     (= (sum-primes-squared-iter 50 60) 6290))

; product-of-relatively-prime : N -> N
; to multiply the positive integers less than n that are relatively prime to n
(define (product-of-relatively-prime n)
  ; relatively-prime? : N -> boolean
  (define (relatively-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 add1 (- n 1)))

(define (product-of-relatively-prime-iter n)
  ; relatively-prime? : N -> boolean
  (define (relatively-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate-iter relatively-prime? * 1 identity 1 add1 (- n 1)))

; Examples/Tests: 
(and (= (product-of-relatively-prime 7) 720)
     (= (product-of-relatively-prime-iter 7) 720))
(and (= (product-of-relatively-prime 8) (* 3 5 7))
     (= (product-of-relatively-prime-iter 8) (* 3 5 7)))
(and (= (product-of-relatively-prime 12) (* 5 7 11))
     (= (product-of-relatively-prime-iter 12) (* 5 7 11)))