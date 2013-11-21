#lang racket

; corner-split : painter N -> painter
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; square-limit : painter N -> painter
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz 
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))