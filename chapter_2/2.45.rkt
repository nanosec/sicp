#lang racket

; A painter is a procedure that draws an image. 

; split : (painter painter -> painter) (painter painter -> painter) -> painter
(define (split operation-1 operation-2)
  ; split-painter : painter N -> painter
  (define (split-painter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-painter painter (- n 1))))
          (operation-1 painter (operation-2 smaller smaller)))))
  split-painter)

; Not implemented: 
; below : painter painter -> painter
; beside : painter painter -> painter

; right-split : painter N -> painter
(define right-split (split beside below))

; up-split : painter N -> painter
(define up-split (split below beside))