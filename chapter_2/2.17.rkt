#lang racket

; last-pair : non-empty-list -> (list X)
; to return a list that contains only the last element of non-empty-list
(define (last-pair non-empty-list)
  (if (null? (cdr non-empty-list))
      non-empty-list
      (last-pair (cdr non-empty-list))))
; Examples/Tests: 
(equal? (last-pair (list 0)) (list 0))
(equal? (last-pair (list 1 2 3)) (list 3))
(equal? (last-pair (list 23 72 149 34)) (list 34))