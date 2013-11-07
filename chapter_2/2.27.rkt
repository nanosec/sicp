#lang racket

; deep-reverse : list -> list
; to deep-reverse a-list
(define (deep-reverse a-list)
  ; insert-at-end : X list -> list
  (define (insert-at-end x a-list)
    (if (null? a-list)
        (list x)
        (cons (car a-list) (insert-at-end x (cdr a-list)))))
  (if (null? a-list)
      a-list
      (if (pair? (car a-list))
          (insert-at-end (deep-reverse (car a-list)) 
                         (deep-reverse (cdr a-list)))
          (insert-at-end (car a-list) 
                         (deep-reverse (cdr a-list))))))
; Examples/Tests: 
(equal? (deep-reverse null) null)
(equal? (deep-reverse (list 1 (list 2 3) 4))
        (list 4 (list 3 2) 1))
(equal? (deep-reverse (list (list 1 2) (list 3 4)))
        (list (list 4 3) (list 2 1)))
(equal? (deep-reverse (list (list 1 2 3) 4 (list 5 6)))
        (list (list 6 5) 4 (list 3 2 1)))