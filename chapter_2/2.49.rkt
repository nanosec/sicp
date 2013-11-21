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

; A segment is a pair: (cons start end), where start and end are vects. 

; make-segment : vect vect -> segment
(define (make-segment start end)
  (cons start end))

; start-segment : segment -> vect
(define (start-segment segment)
  (car segment))

; end-segment : segment -> vect
(define (end-segment segment)
  (cdr segment))

; A painter is a procedure that draws an image. 

; Not implemented: 
; draw-line : vect vect -> void

; segments->painter : (list-of segment) -> painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; draw-outline : frame -> painter
; to draw the outline of frame
(define (draw-outline frame)
  (let ((transform (frame-coord-map frame)))
    (let ((corner1 (transform (make-vect 0 0)))
          (corner2 (transform (make-vect 1 0)))
          (corner3 (transform (make-vect 1 1)))
          (corner4 (transform (make-vect 0 1))))
      (let ((segment1 (make-segment corner1 corner2))
            (segment2 (make-segment corner2 corner3))
            (segment3 (make-segment corner3 corner4))
            (segment4 (make-segment corner4 corner1)))
        (segments->painter (list segment1 segment2 segment3 segment4))))))

; draw-x : frame -> painter
; to draw an "X" by connecting opposite corners of frame
(define (draw-x frame)
  (let ((transform (frame-coord-map frame)))
    (let ((corner1 (transform (make-vect 0 0)))
          (corner2 (transform (make-vect 1 0)))
          (corner3 (transform (make-vect 1 1)))
          (corner4 (transform (make-vect 0 1))))
      (let ((segment1 (make-segment corner1 corner3))
            (segment2 (make-segment corner2 corner4)))
        (segments->painter (list segment1 segment2))))))

; draw-diamond : frame -> painter
; to draw a diamond by connecting the midponts of the sides of frame
(define (draw-diamond frame)
  (let ((transform (frame-coord-map frame)))
    (let ((corner1 (transform (make-vect 0.5 0)))
          (corner2 (transform (make-vect 1 0.5)))
          (corner3 (transform (make-vect 0.5 1)))
          (corner4 (transform (make-vect 0 0.5))))
      (let ((segment1 (make-segment corner1 corner2))
            (segment2 (make-segment corner2 corner3))
            (segment3 (make-segment corner3 corner4))
            (segment4 (make-segment corner4 corner1)))
        (segments->painter (list segment1 segment2 segment3 segment4))))))