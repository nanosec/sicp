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

; equal-point? : point -> boolean
(define (equal-point? point1 point2)
  (and (= (x-point point1) (x-point point2))
       (= (y-point point1) (y-point point2))))
; Examples/Tests: 
(equal-point? (make-point 3 4) (make-point 3 4))
(not (equal-point? (make-point 3 4) (make-point 3 5)))
(not (equal-point? (make-point 2 4) (make-point 3 4)))

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

; length-segment : segment -> #
; to compute the length of segment
(define (length-segment segment)
  (let ((x-start (x-point (start-segment segment)))
        (x-end (x-point (end-segment segment)))
        (y-start (y-point (start-segment segment)))
        (y-end (y-point (end-segment segment))))
    (sqrt (+ (sqr (- x-start x-end))
             (sqr (- y-start y-end))))))
; Examples/Tests: 
(define p1 (make-point 1 2))
(define p2 (make-point 19 26))
(define p3 (make-point -23 34))
(define p4 (make-point -5 58))
(define s1 (make-segment p1 p2))
(define s2 (make-segment p1 p3))
(define s3 (make-segment p2 p4))
(define s4 (make-segment p3 p4))
(= (length-segment s1) 30)
(= (length-segment s2) 40)
(= (length-segment s3) 40)
(= (length-segment s4) 30)

; A rectangle is a pair: (cons side1 side2), 
; where side1 and side2 are segments. 

; Implementation 1: The two sides are adjacent. 
; Implementation 2: The two sides are opposite. 

; make-rectangle : segment segment -> rectangle
; Assumption: side1 and side2 belong to a valid rectangle. 
(define (make-rectangle side1 side2)
  (cons side1 side2))

; side1-rectangle : rectangle -> segment
(define (side1-rectangle rectangle)
  (car rectangle))

; side2-rectangle : rectangle -> segment
(define (side2-rectangle rectangle)
  (cdr rectangle))

; length-rectangle : rectangle -> #
(define (length-rectangle rectangle)
  (length-segment (side1-rectangle rectangle)))

; width-rectangle : rectangle -> #
; note: this handles both implementations of rectangle
(define (width-rectangle rectangle)
  (let ((start-side1 (start-segment (side1-rectangle rectangle)))
        (start-side2 (start-segment (side2-rectangle rectangle)))
        (end-side1 (end-segment (side1-rectangle rectangle)))
        (end-side2 (end-segment (side2-rectangle rectangle))))
    (let ((adjacent-sides? (or (equal-point? start-side1 start-side2)
                               (equal-point? start-side1 end-side2)
                               (equal-point? end-side1 start-side2)
                               (equal-point? end-side1 end-side2))))
      (if adjacent-sides?
          (length-segment (side2-rectangle rectangle))
          (let ((start1-to-start2 (make-segment start-side1 start-side2))
                (start1-to-end2 (make-segment start-side1 end-side2)))
            (min (length-segment start1-to-start2)
                 (length-segment start1-to-end2)))))))
; Examples/Tests: 
(define r1 (make-rectangle s1 s2))
(define r2 (make-rectangle s1 s4))
(= (width-rectangle r1) 40)
(= (width-rectangle r2) 40)

; perimeter-rectangle : rectangle -> #
; to compute the perimeter of rectangle
(define (perimeter-rectangle rectangle)
  (* 2 (+ (length-rectangle rectangle)
          (width-rectangle rectangle))))
; Examples/Tests: 
(= (perimeter-rectangle r1) 140)
(= (perimeter-rectangle r2) 140)

; area-rectangle : rectangle -> #
; to compute the area of rectangle
(define (area-rectangle rectangle)
  (* (length-rectangle rectangle) 
     (width-rectangle rectangle)))
; Examples/Tests: 
(= (area-rectangle r1) 1200)
(= (area-rectangle r2) 1200)