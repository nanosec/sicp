#lang racket

; A rat (rational number) is a pair: (cons x y), where x and y are integers. 

; make-rat : Int Int -> rat
; n = numerator, d = denominator
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((> d 0) (cons (/ n g) (/ d g)))
          ((= d 0) (error "cannot divide by 0"))
          ((< d 0) (cons (- (/ n g)) (- (/ d g)))))))
; Examples/Tests: 
(equal? (make-rat 4 8) (cons 1 2))
(equal? (make-rat -9 6) (cons -3 2))
(equal? (make-rat 3 -12) (cons -1 4))
(equal? (make-rat -6 -10) (cons 3 5))