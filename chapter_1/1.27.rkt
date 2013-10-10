#lang racket

; possible-prime? : N -> boolean
; to determine if n0 is possibly a prime number
(define (possible-prime? n0)
  ; fast-prime? : N N -> boolean
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
  (fast-prime? n0 (- n0 1)))
; Examples/Tests: 
(possible-prime? 199)  ; prime
(possible-prime? 1999)  ; prime
(possible-prime? 561)  ; Carmichael
(possible-prime? 1105)
(possible-prime? 1729)
(possible-prime? 2465)
(possible-prime? 2821)
(possible-prime? 6601)