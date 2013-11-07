#lang racket

; fringe : list -> list
; to flatten a-list
(define (fringe a-list)
  (if (null? a-list)
      a-list
      (if (pair? (car a-list))
          (append (fringe (car a-list))
                  (fringe (cdr a-list)))
          (cons (car a-list) 
                (fringe (cdr a-list))))))
; Examples/Tests: 
(equal? (fringe null) null)
(equal? (fringe (list 1 2 3)) (list 1 2 3))
(define x (list (list 1 2) (list 3 4)))
(equal? (fringe x) (list 1 2 3 4))
(equal? (fringe (list x x)) (list 1 2 3 4 1 2 3 4))
(equal? (fringe (list 1 (list 2 (list 3 4 x)) (list 5 6)))
        (list 1 2 3 4 1 2 3 4 5 6))