#lang racket

; A point is a pair: (cons x y), where x and y are numbers. 

; make-point : # # -> point
(define (make-point x y)
  (cons x y))

; x-point : point -> #
(define (x-point point)
  (car point))

; y-point : point -> #
(define (y-point point)
  (cdr point))

; A segment is a pair: (cons start end), where start and end are points. 

; make-segment : point point -> segment
(define (make-segment start end)
  (cons start end))

; start-segment : segment -> point
(define (start-segment segment)
  (car segment))

; end-segment : segment -> point
(define (end-segment segment)
  (cdr segment))

; mean : # # -> #
(define (mean x y)
  (/ (+ x y) 2))

; midpoint-segment : segment -> point
; to determine the midpoint of segment
(define (midpoint-segment segment)
  (let ((x-start (x-point (start-segment segment)))
        (x-end (x-point (end-segment segment)))
        (y-start (y-point (start-segment segment)))
        (y-end (y-point (end-segment segment))))
    (make-point (mean x-start x-end) (mean y-start y-end))))
; Examples/Tests: 
(define p1 (make-point 3 -5))
(define p2 (make-point 8 2))
(define p3 (make-point -4 7))
(define s1 (make-segment p1 p2))
(define s2 (make-segment p1 p3))
(define s3 (make-segment p2 p3))
(equal? (midpoint-segment s1) (make-point 11/2 -3/2))
(equal? (midpoint-segment s2) (make-point -1/2 1))
(equal? (midpoint-segment s3) (make-point 2 9/2))