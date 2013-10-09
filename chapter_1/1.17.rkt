#lang racket

; fast-* : number N -> number
; to compute a*b using addition, doubling, and halving
(define (fast-* a b)
  (if (= b 0)
      0
      (if (even? b)
          (* 2 (fast-* a (/ b 2)))
          (+ a (fast-* a (- b 1))))))
; Examples/Tests: 
(= (fast-* 0 2) 0)
(= (fast-* 2 0) 0)
(= (fast-* 1 3) 3)
(= (fast-* 17 23) 391)
(= (fast-* 111 222) 24642)
