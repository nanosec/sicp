#lang racket

; fast-* : number N -> number
; to compute a0*b0 iteratively
(define (fast-* a0 b0)
  ; fast-*-iter : number number N -> number
  ; Note: c+a*b remains constant across iterations
  (define (fast-*-iter c a b)
    (if (= b 0)
        c
        (if (even? b) 
            (fast-*-iter c (* 2 a) (/ b 2))
            (fast-*-iter (+ c a) a (- b 1)))))
  (fast-*-iter 0 a0 b0))
; Examples/Tests: 
(= (fast-* 0 2) 0)
(= (fast-* 2 0) 0)
(= (fast-* 1 3) 3)
(= (fast-* 17 23) 391)
(= (fast-* 111 222) 24642)
