#lang racket

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; map : (X -> Y) (list-of X) -> (list-of Y)
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
; Examples/Tests: 
(null? (map sqr null))
(equal? (map sqr (list 1 2 3)) (list 1 4 9))
(equal? (map sqrt (list 16 25 36)) (list 4 5 6))

; append : list list -> list
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
; Examples/Tests: 
(null? (append null null))
(equal? (append (list 1 2 3) null) (list 1 2 3))
(equal? (append null (list 1 2 3)) (list 1 2 3))
(equal? (append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

; length : list -> N
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
; Examples/Tests: 
(= (length null) 0)
(= (length (list 1 3 5)) 3)
(= (length (list 2 4 6 8 10)) 5)