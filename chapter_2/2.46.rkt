#lang racket

; A vect is a pair: (cons xcor ycor), where xcor and ycor are numbers. 

; make-vect : # # -> vect
(define (make-vect xcor ycor)
  (cons xcor ycor))

; xcor-vect : vect -> #
(define (xcor-vect vect)
  (car vect))

; ycor-vect : vect -> #
(define (ycor-vect vect)
  (cdr vect))

; equal-vect? : vect vect -> boolean
(define (equal-vect? v1 v2)
  (and (= (xcor-vect v1) (xcor-vect v2))
       (= (ycor-vect v1) (ycor-vect v2))))
; Examples/Tests: 
(equal-vect? (make-vect 1 2) (make-vect 1 2))
(not (equal-vect? (make-vect 0 2) (make-vect 1 2)))
(not (equal-vect? (make-vect 1 2) (make-vect 1 3)))

; add-vect : vect vect -> vect
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
; Examples/Tests: 
(equal-vect? (add-vect (make-vect 1 2) (make-vect 3 4)) (make-vect 4 6))
(equal-vect? (add-vect (make-vect 5 8) (make-vect 2 7)) (make-vect 7 15))

; sub-vect : vect vect -> vect
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
; Examples/Tests: 
(equal-vect? (sub-vect (make-vect 4 5) (make-vect 2 1)) (make-vect 2 4))
(equal-vect? (sub-vect (make-vect 8 3) (make-vect 5 6)) (make-vect 3 -3))

; scale-vect : # vect -> vect
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
; Examples/Tests: 
(equal-vect? (scale-vect 1 (make-vect 2 3)) (make-vect 2 3))
(equal-vect? (scale-vect 3 (make-vect 5 9)) (make-vect 15 27))