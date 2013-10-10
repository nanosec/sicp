#lang racket

; fast-prime? : N N -> boolean
; to determine if n is prime using the Fermat test a given number of times
(define (fast-prime? n times)
  ; fermat-test : N -> boolean
  (define (fermat-test n)
    ; try-it : N -> boolean
    (define (try-it a)
      ; expmod : N N N -> N
      (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp) (remainder (sqr (expmod base (/ exp 2) m))
                                      m))
              (else (remainder (* base (expmod base (- exp 1) m))
                               m))))
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

; fermat-repetitions : N -> N
; to determine the number of times to repeat the Fermat test
(define (fermat-repetitions n)
  1000)

; list-lowest-3-odd-primes : N -> '(N N N)
; to list the 3 smallest odd primes larger than lower-bound
(define (list-lowest-3-odd-primes lower-bound)
  ; list-primes-acc : N -> '(N N N)
  (define (list-primes-acc n list-of-primes number-found)
    (if (= number-found 3) 
        list-of-primes
        (if (fast-prime? n (fermat-repetitions n)) 
            (list-primes-acc (+ n 2) (cons n list-of-primes) (+ number-found 1))
            (list-primes-acc (+ n 2) list-of-primes number-found))))
  (define odd-n0
    (cond ((< lower-bound 1) 3) 
          ((even? lower-bound) (+ lower-bound 1))
          (else (+ lower-bound 2))))
  (list-primes-acc odd-n0 '() 0))
; Examples/Tests: 
(equal? (list-lowest-3-odd-primes 0) '(7 5 3))
(equal? (list-lowest-3-odd-primes 15) '(23 19 17))
(equal? (list-lowest-3-odd-primes 50) '(61 59 53))

(list-lowest-3-odd-primes 1000)
(list-lowest-3-odd-primes 10000)
(list-lowest-3-odd-primes 100000)
(list-lowest-3-odd-primes 1000000)

; time-fast-prime? : N -> N
; to time (fast-prime? n) in ms, only for prime n
(define (time-prime? n)
  (define start-time (current-milliseconds))
  (if (fast-prime? n (fermat-repetitions n))
      (- (current-milliseconds) start-time)
      (error "input is not prime")))

; primes->times : (listof N) -> (listof N)
; to convert list-of-primes to a list of times using time-prime?
(define (primes->times list-of-primes)
  (cond 
    ((empty? list-of-primes) empty)
    (else (cons (time-prime? (first list-of-primes))
                (primes->times (rest list-of-primes))))))

(primes->times (list-lowest-3-odd-primes 1000))
(primes->times (list-lowest-3-odd-primes 1000000))
(primes->times (list-lowest-3-odd-primes 1000000000))
