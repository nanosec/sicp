#lang racket

; reverse : list -> list
; to reverse a-list
(define (reverse a-list)
  ; insert-at-end : X list-> list
  (define (insert-at-end x a-list)
    (if (null? a-list)
        (list x)
        (cons (car a-list) (insert-at-end x (cdr a-list)))))
  (if (null? a-list)
      a-list
      (insert-at-end (car a-list) (reverse (cdr a-list)))))
; Examples/Tests: 
(equal? (reverse null) null)
(equal? (reverse (list 1)) (list 1))
(equal? (reverse (list 1 2 3)) (list 3 2 1))
(equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))