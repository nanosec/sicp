#lang racket

; cons : N N -> N[>0]
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
; Examples/Tests: 
(= (cons 0 0) 1)
(= (cons 1 2) 18)
(= (cons 3 2) 72)

; find-exponent : N[>0] N -> N
; to find the exponent of base in the factorization of product0
(define (find-exponent product0 base)
  ; find-exponent-iter : N[>0] N -> N
  (define (find-exponent-iter product exponent)
    (if (= (modulo product base) 0)
        (find-exponent-iter (/ product base) (+ exponent 1))
        exponent))
  (find-exponent-iter product0 0))
; Examples/Tests: 
(= (find-exponent 9 2) 0)
(= (find-exponent 16 3) 0)
(= (find-exponent 24 2) 3)
(= (find-exponent 1134 3) 4)

; car : N[>0] -> N
(define (car n)
  (find-exponent n 2))
; Examples/Tests: 
(= (car (cons 0 0)) 0)
(= (car (cons 0 3)) 0)
(= (car (cons 4 6)) 4)
(= (car (cons 8 7)) 8)

; cdr : N[>0] -> N
(define (cdr n)
  (find-exponent n 3))
; Examples/Tests: 
(= (cdr (cons 0 0)) 0)
(= (cdr (cons 3 0)) 0)
(= (cdr (cons 9 6)) 6)
(= (cdr (cons 5 8)) 8)