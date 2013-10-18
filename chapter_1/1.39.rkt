#lang racket

; cont-frac : (N -> #) (N -> #) N -> #
; to calculate the k-term finite continued fraction, where 
; (n i) and (d i) produce the ith numerator and denominator
(define (cont-frac n d k)
  ; cont-frac-recur : N -> #
  (define (cont-frac-recur i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cont-frac-recur (+ i 1))))))
  (cont-frac-recur 1))

; cont-frac-iter : (N -> #) (N -> #) N -> #
(define (cont-frac-iter n d k)
  ; cont-frac-iter2 : N # -> #
  (define (cont-frac-iter2 i accumulator)
    (if (= i 1)
        (/ (n i) (+ (d i) 
                    accumulator))
        (cont-frac-iter2 (- i 1) (/ (n i) 
                                    (+ (d i) accumulator)))))
  (cont-frac-iter2 k 0))

; Examples/Tests: 

; tan-cf : # N -> #
(define (tan-cf x k)
  ; n : N -> #
  (define (n i)
    (if (= i 1)
        x
        (- (sqr x))))
  ; d : N -> #
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))
; Examples/Tests: 
(= (tan-cf 0 1) 0)
(= (tan-cf 2 3) -22/9)
(= (tan-cf -17 5) -920057/1132380)
(tan-cf pi 10) ; 0
(tan-cf (/ pi 4) 10) ; 1