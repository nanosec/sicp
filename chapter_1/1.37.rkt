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
(let ((n (lambda (i) 1))
      (d (lambda (i) 2)))
  (and (= (cont-frac n d 3) 5/12)
       (= (cont-frac-iter n d 3) 5/12)))
(let ((n (lambda (i) 23))
      (d (lambda (i) 17)))
  (and (= (cont-frac n d 4) 130985/103991)
       (= (cont-frac-iter n d 4) 130985/103991)))
(define (one i) 1.0)
(and (= (cont-frac one one 5) (/ 5 8))
     (= (cont-frac-iter one one 5) (/ 5 8)))

(define inverse-phi (/ 2 (+ 1 (sqrt 5))))
(- (cont-frac one one 10) inverse-phi)