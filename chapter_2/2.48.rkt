#lang racket

; A vect is a pair: (cons xcor ycor), where xcor and ycor are numbers. 

; A segment is a pair: (cons start end), where start and end are vects. 

; make-segment : vect vect -> segment
(define (make-segment start end)
  (cons start end))

; start-segment : segment -> vect
(define (start-segment segment)
  (car segment))

; end-segment : segment -> vect
(define (end-segment segment)
  (cdr segment))