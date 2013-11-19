#lang racket

; A painter is a procedure that draws an image. 

; Not implemented: 
; below : painter painter -> painter
; beside : painter painter -> painter

; up-split : painter N -> painter
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))