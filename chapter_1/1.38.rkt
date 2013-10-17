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
(define (n i) 1)
(define (d i)
  (if (= (modulo i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))
(- (cont-frac n d 10) (exp 1) -2)