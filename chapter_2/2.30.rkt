#lang racket

; A tree is either 
; 1. null, 
; 2. (cons tree tree), or 
; 3. (cons # tree). 

; square-tree : tree -> tree
; to produce a tree with its numbers squared relative to a-tree
(define (square-tree a-tree)
  (cond ((null? a-tree) null)
        ((pair? (car a-tree)) (cons (square-tree (car a-tree))
                                    (square-tree (cdr a-tree))))
        (else (cons (sqr (car a-tree))
                    (square-tree (cdr a-tree))))))
; Examples/Tests: 
(equal? (square-tree null) null)
(equal? (square-tree (list 1 2 3)) (list 1 4 9))
(equal? (square-tree (list (list 1 2) 3 (list 4 5) 6))
        (list (list 1 4) 9 (list 16 25) 36))
(equal? (square-tree (list 1 (list 2 3 (list 4)) (list 5 6)))
        (list 1 (list 4 9 (list 16)) (list 25 36)))
(equal? (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
        (list 1 (list 4 (list 9 16) 25) (list 36 49)))

; square-tree-map : tree -> tree
; a version of square-tree that uses map
(define (square-tree-map a-tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (sqr x)))
       a-tree))
; Examples/Tests: 
(equal? (square-tree-map null) null)
(equal? (square-tree-map (list 1 2 3)) (list 1 4 9))
(equal? (square-tree-map (list (list 1 2) 3 (list 4 5) 6))
        (list (list 1 4) 9 (list 16 25) 36))
(equal? (square-tree-map (list 1 (list 2 3 (list 4)) (list 5 6)))
        (list 1 (list 4 9 (list 16)) (list 25 36)))
(equal? (square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
        (list 1 (list 4 (list 9 16) 25) (list 36 49)))