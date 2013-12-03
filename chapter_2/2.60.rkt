#lang racket

; A set is a list that may have duplicates. 

; element-of-set? : X set -> boolean
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

; adjoin-set : X set -> set
(define (adjoin-set x set)
  (cons x set))
; Examples/Tests: 
(equal? (adjoin-set 1 null) '(1))
(equal? (adjoin-set 1 '(2)) '(1 2))
(equal? (adjoin-set 1 '(3 2 1)) '(1 3 2 1))
(equal? (adjoin-set 1 '(1 2 3 5)) '(1 1 2 3 5))

; intersection-set : set set -> set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

; union-set : set set -> set
(define (union-set set1 set2)
  (append set1 set2))
; Examples/Tests: 
(null? (union-set null null))
(equal? (union-set null '(1 2 3)) '(1 2 3))
(equal? (union-set '(1 2 3) null) '(1 2 3))
(equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(equal? (union-set '(1 2 3 4) '(2 4 6 8)) '(1 2 3 4 2 4 6 8))
(equal? (union-set '(1 2 3 4) '(7 5 3 1)) '(1 2 3 4 7 5 3 1))
