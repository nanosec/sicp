#lang racket

; square-list-1 : (list-of #) -> (list-of #)
(define (square-list-1 items)
  (if (null? items)
      null
      (cons (sqr (car items))
            (square-list-1 (cdr items)))))

; square-list-2 : (list-of #) -> (list-of #)
(define (square-list-2 items)
  (map sqr items))

; Examples/Tests: 
(and (equal? (square-list-1 null) null)
     (equal? (square-list-2 null) null))
(and (equal? (square-list-1 (list 1 2 3 4)) (list 1 4 9 16))
     (equal? (square-list-2 (list 1 2 3 4)) (list 1 4 9 16)))