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

; mul-interval : interval interval -> interval
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; div-interval : interval interval -> interval
(define (div-interval x y)
  ; div-interval-aux : interval interval -> interval
  (define (div-interval-aux x y)
    (mul-interval x 
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))
  (let ((y-spans-0? (and (<= (lower-bound y) 0)
                         (<= 0 (upper-bound y)))))
    (if y-spans-0?
        (error "dividing by interval that spans 0")
        (div-interval-aux x y))))
; Examples/Tests: 
(equal-interval? (div-interval (make-interval 12 18) (make-interval 3 6))
                 (make-interval 2 6))
(div-interval (make-interval 12 18) (make-interval -3 6)) ; error