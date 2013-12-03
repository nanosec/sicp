#lang racket

; A set is a list with no duplicates. 

; element-of-set? : X set -> boolean
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; union-set : set set -> set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (if (element-of-set? (car set1) set2)
		  (union-set (cdr set1) set2)
		  (cons (car set1)
			(union-set (cdr set1) set2))))))
; Examples/Tests: 
(null? (union-set null null))
(equal? (union-set null '(1 2 3)) '(1 2 3))
(equal? (union-set '(1 2 3) null) '(1 2 3))
(equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(equal? (union-set '(1 2 3 4) '(2 4 6 8)) '(1 3 2 4 6 8))
(equal? (union-set '(1 2 3 4) '(7 5 3 1)) '(2 4 7 5 3 1))
