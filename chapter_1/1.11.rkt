#lang racket

; f(n) = { n, if n < 3
;        { f(n-1) + 2*f(n-2) + 3*f(n-3), if n >= 3

; f-recursive : number -> number
; to compute f(n) by means of a recursive process
(define (f-recursive n)
  (cond 
    [(< n 3) n]
    [else (+ (f-recursive (- n 1))
             (* 2 (f-recursive (- n 2)))
             (* 3 (f-recursive (- n 3))))]))
; Examples/Tests: 
(= (f-recursive -3.14) -3.14)
(= (f-recursive 2.99) 2.99)
(= (f-recursive 3) 4)
(= (f-recursive 4) 11)
(= (f-recursive 5) 25)
(= (f-recursive 4.5) 16.5)
(= (f-recursive 5.25) 31.5)

; f-iterative : number -> number
; to compute f(n) by means of an iterative process
(define (f-iterative n)  
  ; f-iter : number number number number -> number
  (define (f-iter counter f_n-1 f_n-2 f_n-3)
    (define f_n 
      (+ f_n-1 (* 2 f_n-2) (* 3 f_n-3)))
    (cond 
      [(< counter 3) f_n]
      [else (f-iter (- counter 1) f_n f_n-1 f_n-2)]))
  
  (cond 
    [(< n 3) n]
    [else (define n-fraction (- n (floor n)))
          (f-iter (- n 1) 
                  (+ 2 n-fraction) 
                  (+ 1 n-fraction) 
                  n-fraction)]))
; Examples/Tests: 
(= (f-iterative -3.14) -3.14)
(= (f-iterative 2.99) 2.99)
(= (f-iterative 3) 4)
(= (f-iterative 4) 11)
(= (f-iterative 5) 25)
(= (f-iterative 4.5) 16.5)
(= (f-iterative 5.25) 31.5)
