#lang racket

; A set is an ordered list with no duplicates. 

; adjoin-set : # set -> set
(define (adjoin-set x set)
  (if (null? set)
      (cons x set)
      (cond ((< x (car set)) (cons x set))
	    ((= x (car set)) set)
	    (else (cons (car set) (adjoin-set x (cdr set)))))))
; Examples/Tests: 
(equal? (adjoin-set 1 null) '(1))
(equal? (adjoin-set 3 '(1 2 3 4)) '(1 2 3 4))
(equal? (adjoin-set 3 '(1 2 4 5)) '(1 2 3 4 5))
