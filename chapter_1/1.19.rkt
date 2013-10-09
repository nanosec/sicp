#lang racket

; T(p,q) : (a,b) -> (bq + aq + ap, bp + aq)
; T(p,q)T(p,q) : (a,b) -> ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, 
;                          (bp + aq)p + (bq + aq + ap)q)
;  = (bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2, 
;     bp^2 + aqp + bq^2 + aq^2 + apq)
;  = (2aq^2 + 2bpq + 2apq + ap^2 + bq^2, bp^2 + bq^2 + 2apq + aq^2)
; T(p',q') : (a,b) -> (bq' + aq' + ap', bp' + aq')
; p' = p^2 + q^2, q' = 2pq + q^2

; fib : N -> N
; to compute the nth Fibonacci number
(define (fib n)
  ; fib-iter : N N N N N -> N
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (sqr p) (sqr q))
                     (+ (* 2 p q) (sqr q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
; Examples/Tests: 
(= (fib 0) 0)
(= (fib 11) 89)
(= (fib 14) 377)
(= (fib 17) 1597)
(= (fib 20) 6765)

