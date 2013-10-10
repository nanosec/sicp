#lang racket

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

; list-lowest-3-odd-primes : N -> '(N N N)
; to list the 3 smallest odd primes larger than lower-bound
(define (list-lowest-3-odd-primes lower-bound)
  ; list-primes-acc : N -> '(N N N)
  (define (list-primes-acc n list-of-primes number-found)
    (if (= number-found 3) 
        list-of-primes
        (if (prime? n) 
            (list-primes-acc (+ n 2) (cons n list-of-primes) (+ number-found 1))
            (list-primes-acc (+ n 2) list-of-primes number-found))))
  (define odd-n0
    (if (even? lower-bound)
        (+ lower-bound 1)
        (+ lower-bound 2)))
  (list-primes-acc odd-n0 '() 0))
; Examples/Tests: 
(equal? (list-lowest-3-odd-primes 0) '(7 5 3))
(equal? (list-lowest-3-odd-primes 15) '(23 19 17))
(equal? (list-lowest-3-odd-primes 50) '(61 59 53))

(list-lowest-3-odd-primes 1e3)
(list-lowest-3-odd-primes 1e4)
(list-lowest-3-odd-primes 1e5)
(list-lowest-3-odd-primes 1e6)

; time-prime? : N -> N
; to time (prime? n) in ms, only for prime n
(define (time-prime? n)
  (define start-time (current-milliseconds))
  (if (prime? n)
      (- (current-milliseconds) start-time)
      (error "input is not prime")))

; primes->times : (listof N) -> (listof N)
; to convert list-of-primes to a list of times using time-prime?
(define (primes->times list-of-primes)
  (cond 
    ((empty? list-of-primes) empty)
    (else (cons (time-prime? (first list-of-primes))
                (primes->times (rest list-of-primes))))))

(primes->times (list-lowest-3-odd-primes 1e7))
(primes->times (list-lowest-3-odd-primes 1e8))
(primes->times (list-lowest-3-odd-primes 1e9))