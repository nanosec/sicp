#lang racket

; fast-expt : number N -> number
; to compute base^exponent iteratively
(define (fast-expt base exponent)
  ; fast-expt-iter : number number N -> number
  ; Note: a*b^n remains constant across iterations
  (define (fast-expt-iter a b n)
    (if (= n 0)
        a
        (if (even? n) 
            (fast-expt-iter a (sqr b) (/ n 2))
            (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 base exponent))
; Examples/Tests: 
(= (fast-expt 2 0) 1)
(= (fast-expt 2 6) 64)
(= (fast-expt 2 11) 2048)
(= (fast-expt 3 7) 2187)
(= (fast-expt 3 10) 59049)
