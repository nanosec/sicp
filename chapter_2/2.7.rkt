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
; Examples/Tests: 
(= (lower-bound (make-interval 1 2)) 1)
(= (lower-bound (make-interval 3 4)) 3)

; upper-bound : interval -> #
(define (upper-bound interval)
  (cdr interval))
; Examples/Tests: 
(= (upper-bound (make-interval 1 2)) 2)
(= (upper-bound (make-interval 3 4)) 4)