#lang racket

; A set is an ordered list with no duplicates. 

; union-set : set set -> set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		      ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
		      (else (cons x2 (union-set set1 (cdr set2)))))))))
; Examples/Tests: 
(null? (union-set null null))
(equal? (union-set null '(1 2 3)) '(1 2 3))
(equal? (union-set '(1 2 3) null) '(1 2 3))
(equal? (union-set '(1 3 5) '(2 3 4)) '(1 2 3 4 5))
(equal? (union-set '(2 4 6) '(1 2 3 4 5)) '(1 2 3 4 5 6))
