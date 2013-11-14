#lang racket

; enumerate-interval : # # -> (list-of #)
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; flatmap : (X -> (list-of Y)) (list-of X) -> (list-of Y)
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

; ordered-triples : # -> (list-of (list N+ N+ N+))
; to generate the sequence of pairs (i,j,k) with 1 <= k < j < i <= n
(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
; Examples/Tests:
(null? (ordered-triples 2))
(equal? (ordered-triples 3) (list (list 3 2 1)))
(equal? (ordered-triples 4) 
        (list (list 3 2 1) (list 4 2 1) (list 4 3 1) (list 4 3 2)))

; ordered-triples-summing-to : # -> (list N+ N+ N+)
(define (ordered-triples-summing-to n)
  ; triple-sums-to-n? : (list N+ N+ N+) -> boolean
  (define (triple-sums-to-n? triple)
    (= (accumulate + 0 triple) n))
  (filter triple-sums-to-n? (ordered-triples n)))
; Examples/Tests: 
(null? (ordered-triples-summing-to 5))
(equal? (ordered-triples-summing-to 6) (list (list 3 2 1)))
(equal? (ordered-triples-summing-to 7) (list (list 4 2 1)))
(equal? (ordered-triples-summing-to 8) (list (list 4 3 1) (list 5 2 1)))
(equal? (ordered-triples-summing-to 9)
        (list (list 4 3 2) (list 5 3 1) (list 6 2 1)))
(equal? (ordered-triples-summing-to 10)
        (list (list 5 3 2) (list 5 4 1) (list 6 3 1) (list 7 2 1)))