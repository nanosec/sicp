#lang racket

; An interval is a pair: (cons lower-bound upper-bound), 
; where lower-bound and upper-bound are numbers. 

; Assumption: lower-bound <= upper-bound

; make-interval : # # -> interval
(define (make-interval a b) 
  (cons a b))

; lower-bound : interval -> #
(define (lower-bound interval)
  (car interval))

; upper-bound : interval -> #
(define (upper-bound interval)
  (cdr interval))

; equal-interval? : interval interval -> boolean
(define (equal-interval? interval-1 interval-2)
  (and (= (lower-bound interval-1) (lower-bound interval-2))
       (= (upper-bound interval-1) (upper-bound interval-2))))

; make-center-width : # #[>= 0] -> interval
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; center : interval -> #
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; width : interval -> #[>= 0]
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; make-center-percent : #[>= 0] #[>= 0] -> interval
(define (make-center-percent center percent)
  (let ((width (* 0.01 percent center)))
    (make-center-width center width)))
; Examples/Tests: 
(equal-interval? (make-center-percent 10 20) (make-interval 8 12))
(equal-interval? (make-center-percent 40 15) (make-interval 34 46))
(equal-interval? (make-center-percent 60 10) (make-interval 54 66))

; percent : interval -> #[>= 0]
(define (percent interval)
  (* 100 (/ (width interval) (center interval))))
; Examples/Tests: 
(= (percent (make-interval 8 12)) 20)
(= (percent (make-interval 34 46)) 15)
(= (percent (make-interval 54 66)) 10)