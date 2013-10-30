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

; mul-interval : interval interval -> interval
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; mul-interval-alt : interval interval -> interval
(define (mul-interval-alt x y)
  ; negative-interval? : interval -> boolean
  (define (negative-interval? interval)
    (< (upper-bound interval) 0))
  ; positive-interval? : interval -> boolean
  (define (positive-interval? interval)
    (> (lower-bound interval) 0))
  (cond 
    ((negative-interval? x) 
     (cond ((negative-interval? y)
            (make-interval (* (upper-bound x) (upper-bound y))
                           (* (lower-bound x) (lower-bound y))))
           ((positive-interval? y) 
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (upper-bound x) (lower-bound y))))
           (else (make-interval (* (lower-bound x) (upper-bound y))
                                (* (lower-bound x) (lower-bound y))))))
    
    ((positive-interval? x) 
     (cond ((negative-interval? y)
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (lower-bound x) (upper-bound y))))
           ((positive-interval? y)
            (make-interval (* (lower-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y))))
           (else (make-interval (* (upper-bound x) (lower-bound y))
                                (* (upper-bound x) (upper-bound y))))))
    (else 
     (cond ((negative-interval? y) 
            (make-interval (* (upper-bound x) (lower-bound y))
                           (* (lower-bound x) (lower-bound y))))
           ((positive-interval? y) 
            (make-interval (* (lower-bound x) (upper-bound y))
                           (* (upper-bound x) (upper-bound y))))
           (else (make-interval (min (* (lower-bound x) (upper-bound y))
                                     (* (upper-bound x) (lower-bound y)))
                                (max (* (lower-bound x) (lower-bound y))
                                     (* (upper-bound x) (upper-bound y)))))))))
; Examples/Tests: 

; test-mul-interval-alt : interval interval -> boolean
(define (test-mul-interval-alt interval-1 interval-2)
  ; test-mul-interval-alt-aux : interval interval -> boolean
  (define (test-mul-interval-alt-aux interval-1 interval-2)
    (equal-interval? (mul-interval interval-1 interval-2)
                     (mul-interval-alt interval-1 interval-2)))
  (and (test-mul-interval-alt-aux interval-1 interval-2)
       (test-mul-interval-alt-aux interval-2 interval-1)))

(define negative-1 (make-interval -12 -5))
(define negative-2 (make-interval -7 -3))
(define zero-1 (make-interval -6 8))
(define zero-2 (make-interval -9 2))
(define positive-1 (make-interval 4 17))
(define positive-2 (make-interval 1 18))

(test-mul-interval-alt negative-1 negative-2)
(test-mul-interval-alt negative-1 zero-1)
(test-mul-interval-alt negative-1 zero-2)
(test-mul-interval-alt negative-1 positive-1)
(test-mul-interval-alt negative-1 positive-2)
(test-mul-interval-alt negative-2 zero-1)
(test-mul-interval-alt negative-2 zero-2)
(test-mul-interval-alt negative-2 positive-1)
(test-mul-interval-alt negative-2 positive-2)
(test-mul-interval-alt zero-1 zero-2)
(test-mul-interval-alt zero-1 positive-1)
(test-mul-interval-alt zero-1 positive-2)
(test-mul-interval-alt zero-2 positive-1)
(test-mul-interval-alt zero-2 positive-2)
(test-mul-interval-alt positive-1 positive-2)