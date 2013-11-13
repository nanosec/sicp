#lang racket

; fold-right : (X Y -> Y) X (list-of X) -> Y
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; reverse-fold-right : list -> list
; to reverse sequence using fold-right
(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) 
              null
              sequence))
; Examples/Tests: 
(null? (reverse-fold-right null))
(equal? (reverse-fold-right (list 1)) (list 1))
(equal? (reverse-fold-right (list 1 2 3)) (list 3 2 1))
(equal? (reverse-fold-right (list 1 4 9 16 25)) (list 25 16 9 4 1))

; fold-left : (Y X -> Y) X (list-of X) -> Y
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; reverse-fold-left : list -> list
; to reverse sequence using fold-left
(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x))
             null
             sequence))
; Examples/Tests: 
(null? (reverse-fold-left null))
(equal? (reverse-fold-left (list 1)) (list 1))
(equal? (reverse-fold-left (list 1 2 3)) (list 3 2 1))
(equal? (reverse-fold-left (list 1 4 9 16 25)) (list 25 16 9 4 1))