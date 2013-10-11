#lang racket

; Miller-Rabin : N -> boolean
; to determine if n0 is a prime number via the Miller-Rabin test
(define (Miller-Rabin n0)
  ; fast-prime? : N N -> boolean
  (define (fast-prime? n times)
    ; fermat-test : N -> boolean
    (define (fermat-test n)
      ; try-it : N -> boolean
      (define (try-it a)
        ; expmod : N N N -> N
        (define (expmod base exp m)
          (cond ((= exp 0) 1)
                ((even? exp) (define exp/2-result (expmod base (/ exp 2) m))
                             (define exp-result (remainder (sqr exp/2-result) m))
                             (cond ((and (not (= exp/2-result 1))
                                         (not (= exp/2-result (- m 1)))
                                         (= exp-result 1))
                                    0)
                                   (else exp-result)))
                (else (remainder (* base (expmod base (- exp 1) m))
                                 m))))
        (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (cond ((< n0 2) #f)
        (else (fast-prime? n0 (- n0 1)))))
; Examples/Tests: 
(Miller-Rabin 199)
(Miller-Rabin 1999)
(not (Miller-Rabin 19999))
(not (Miller-Rabin 561))
(not (Miller-Rabin 1105))
(not (Miller-Rabin 1729))
(not (Miller-Rabin 2465))
(not (Miller-Rabin 2821))
(not (Miller-Rabin 6601))