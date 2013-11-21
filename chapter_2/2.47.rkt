#lang racket

; A vect is a pair: (cons xcor ycor), where xcor and ycor are numbers. 

; Two implementations of frame: frame-list and frame-cons. 

; A frame-list is a list: (list origin edge1 edge2), 
; where origin, edge1, and edge2 are vects. 

; make-frame-list : vect vect vect -> frame
(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

; origin-frame-list : frame-list -> vect
(define (origin-frame-list frame)
  (car frame))

; edge1-frame-list : frame-list -> vect
(define (edge1-frame-list frame)
  (cadr frame))

; edge2-frame-list : frame-list -> vect
(define (edge2-frame-list frame)
  (caddr frame))

; A frame-cons is a pair: (cons origin (cons edge1 edge2)), 
; where origin, edge1, and edge2 are vects. 

; make-frame-cons : vect vect vect -> frame
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; origin-frame-cons : frame-cons -> vect
(define (origin-frame-cons frame)
  (car frame))

; edge1-frame-cons : frame-cons -> vect
(define (edge1-frame-cons frame)
  (cadr frame))

; edge2-frame-cons : frame-cons -> vect
(define (edge2-frame-cons frame)
  (cddr frame))