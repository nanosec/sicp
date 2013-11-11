#lang racket

; A set is a list. 

; subsets : set -> (set-of set)
; to produce the set of all subsets of the set s
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x) (cons (car s) x)) 
                     rest)))))
; Examples/Tests: 
(equal? (subsets null) (list null))
(equal? (subsets (list 1)) (list null (list 1)))
(equal? (subsets (list 1 2)) 
        (list null (list 2) (list 1) (list 1 2)))
(equal? (subsets (list 1 2 3))
        (list null 
              (list 3) 
              (list 2) 
              (list 2 3) 
              (list 1) 
              (list 1 3) 
              (list 1 2) 
              (list 1 2 3)))