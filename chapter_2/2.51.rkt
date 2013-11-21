#lang racket

; A vect is a pair: (cons xcor ycor), where xcor and ycor are numbers. 

; make-vect : # # -> vect
(define (make-vect xcor ycor)
  (cons xcor ycor))

; xcor-vect : vect -> #
(define (xcor-vect vect)
  (car vect))

; ycor-vect : vect -> #
(define (ycor-vect vect)
  (cdr vect))

; add-vect : vect vect -> vect
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

; sub-vect : vect vect -> vect
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

; scale-vect : # vect -> vect
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; A frame is a list: (list origin edge1 edge2), 
; where origin, edge1, and edge2 are vects. 

; make-frame : vect vect vect -> frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

; origin-frame : frame -> vect
(define (origin-frame frame)
  (car frame))

; edge1-frame : frame -> vect
(define (edge1-frame frame)
  (cadr frame))

; edge2-frame : frame -> vect
(define (edge2-frame frame)
  (caddr frame))

; frame-coord-map : frame -> (vect -> vect)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; A painter is a procedure that draws an image. 

; transform-painter : painter vect vect vect -> painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; rotate90 : painter -> painter
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; rotate270 : painter -> painter
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; beside : painter painter -> painter
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; below : painter painter -> painter
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; below-v2 : painter painter -> painter
(define (below-v2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))