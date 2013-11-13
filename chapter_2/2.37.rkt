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

; A vector is a (non-empty-list-of #). 
; A matrix is a (non-empty-list-of vector). 

(define I (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define B (list (list 9 8 7) (list 6 5 4) (list 3 2 1)))

; dot-product : vector vector -> #
; Assumption: Vectors v and w have the same length. 
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; matrix-*-vector : matrix vector -> vector
; Assumption: Vector v and the rows of matrix m have the same length. 
(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))
; Examples/Tests: 
(equal? (matrix-*-vector (list (list 2)) (list 3)) (list 6))
(equal? (matrix-*-vector I (list 1 4 9)) (list 1 4 9))
(equal? (matrix-*-vector A (list 2 3 5)) (list 23 53 83))
(equal? (matrix-*-vector B (list 5 3 2)) (list 83 53 23))

; transpose : matrix -> matrix
(define (transpose mat)
  (accumulate-n cons null mat))
; Examples/Tests:
(equal? (transpose (list (list 1))) (list (list 1)))
(equal? (transpose (list (list 1 2))) (list (list 1) (list 2)))
(equal? (transpose I) I)
(equal? (transpose A) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
(equal? (transpose B) (list (list 9 6 3) (list 8 5 2) (list 7 4 1)))

; matrix-*-matrix : matrix matrix -> matrix
; Assumption: # of columns of m = # of rows of n
(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n-cols m-row)) m)))
; Examples/Tests: 
(equal? (matrix-*-matrix (list (list 2)) (list (list 3))) (list (list 6)))
(equal? (matrix-*-matrix I A) A)
(equal? (matrix-*-matrix A I) A)
(equal? (matrix-*-matrix A (transpose A))
        (list (list 14 32 50) (list 32 77 122) (list 50 122 194)))
(equal? (matrix-*-matrix (transpose A) A)
        (list (list 66 78 90) (list 78 93 108) (list 90 108 126)))
(equal? (matrix-*-matrix A B)
        (list (list 30 24 18) (list 84 69 54) (list 138 114 90)))
(equal? (matrix-*-matrix B A)
        (list (list 90 114 138) (list 54 69 84) (list 18 24 30)))