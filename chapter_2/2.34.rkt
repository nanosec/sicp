#lang racket

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; horner-eval : # (list-of #) -> #
; to evaluate the polynomial a_0 + a_1*x + ... + a_(n-1)*x^(n-1) + a_n*x^n,
; given x and the coefficient-sequence (list a_0 a_1 ... a_(n-1) a_n),
; using Horner's rule: (...(a_n*x + a_(n-1))*x + ... + a_1)*x + a_0
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))
; Examples/Tests: 
(= (horner-eval 1 (list 5 2 3 7)) 17)
(= (horner-eval 3 (list 10 8 6 4 2)) 358)
(= (horner-eval 2 (list 1 3 0 5 0 1)) 79)