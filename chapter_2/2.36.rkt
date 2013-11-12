#lang racket

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; accumulate-n : (X Y -> Y) X (non-empty-list-of (list-of X)) -> (list-of Y)
; Assumption: All the internal lists of seqs have the same length. 
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; Examples/Tests: 
(equal? (accumulate-n + 0 (list null null)) null)
(equal? (accumulate-n + 0 (list (list 1 2 3))) (list 1 2 3))
(equal? (accumulate-n * 1 (list (list 2 3 4 5) 
                                (list 4 9 16 25) 
                                (list 1 -1 1 -1)))
        (list 8 -27 64 -125))
(equal? (accumulate-n + 0 (list (list 1 2 3) 
                                (list 4 5 6) 
                                (list 7 8 9) 
                                (list 10 11 12)))
        (list 22 26 30))