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

; add-interval : interval interval -> interval
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; mul-interval : interval interval -> interval
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; width-interval : interval -> #
; to determine the width of an interval
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2))
; Examples/Tests: 
(= (width-interval (make-interval 3 3)) 0)
(= (width-interval (make-interval 2 6)) 2)
(= (width-interval (make-interval -4 7)) 11/2)

; width-of-sum : interval interval -> #
; to determine the width of the sum of interval-1 and interval-2
(define (width-of-sum interval-1 interval-2)
  (+ (width-interval interval-1) (width-interval interval-2)))
; Examples/Tests: 
(let ((x (make-interval 2 6))
      (y (make-interval 5 15)))
  (= (width-of-sum x y) 
     (width-interval (add-interval x y))))
(let ((x (make-interval -12 4))
      (y (make-interval -17 -5)))
  (= (width-of-sum x y)
     (width-interval (add-interval x y))))

; width-of-product : interval interval -> #
; to determine the width of the product of interval-1 and interval-2
(define (width-of-product interval-1 interval-2)
  (width-interval (mul-interval interval-1 interval-2)))
; Examples/Tests: 
(let ((x (make-interval 3 9))
      (y (make-interval 8 20)))
  (and (= (width-interval x) 3)
       (= (width-interval y) 6)
       (= (width-of-product x y) 78)))
(let ((x (make-interval -2 4))
      (y (make-interval -7 5)))
  (and (= (width-interval x) 3)
       (= (width-interval y) 6)
       (= (width-of-product x y) 24)))