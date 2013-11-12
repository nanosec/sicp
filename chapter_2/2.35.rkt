#lang racket

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; A tree of numbers is either 
; 1. null, 
; 2. (cons # tree), or
; 3. (cons tree tree). 

; enumerate-tree : tree -> (list-of #)
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; count-leaves : tree -> #
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))
; Examples/Tests: 
(= (count-leaves null) 0)
(= (count-leaves (list 1 5 9)) 3)
(= (count-leaves (list (list 11 13) 17 19)) 4)
(= (count-leaves (list 2 (list (list 4 6) 8) (list 9 (list 7 5)) (list 3))) 8)