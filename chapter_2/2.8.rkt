#lang racket

; An interval is a pair: (cons lower-bound upper-bound), 
; where lower-bound and upper-bound are numbers. 

; Assumption: lower-bound <= upper-bound

; make-interval : # # -> interval
(define (make-interval a b) 
  (cons a b))

; lower-bound : interval -> #
(define (lower-bound interval)
  (car interval))

; upper-bound : interval -> #
(define (upper-bound interval)
  (cdr interval))

; equal-interval? : interval interval -> boolean
(define (equal-interval? interval-1 interval-2)
  (and (= (lower-bound interval-1) (lower-bound interval-2))
       (= (upper-bound interval-1) (upper-bound interval-2))))
; Examples/Tests: 
(equal-interval? (make-interval 3 4) (make-interval 3 4))
(not (equal-interval? (make-interval 3 4) (make-interval 2 4)))
(not (equal-interval? (make-interval 3 5) (make-interval 3 4)))

; add-interval : interval interval -> interval
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; sub-interval : interval interval -> interval
(define (sub-interval interval-1 interval-2)
  (add-interval interval-1 
                (make-interval (- (upper-bound interval-2))
                               (- (lower-bound interval-2)))))
; Examples/Tests: 
(equal-interval? (sub-interval (make-interval 15 18) (make-interval 4 6))
                 (make-interval 9 14))
(equal-interval? (sub-interval (make-interval 4 6) (make-interval 15 18))
                 (make-interval -14 -9))
(equal-interval? (sub-interval (make-interval 3 9) (make-interval 5 15))
                 (make-interval -12 4))
(equal-interval? (sub-interval (make-interval 8 11) (make-interval 2 17))
                 (make-interval -9 9))