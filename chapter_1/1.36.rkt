#lang racket

(define tolerance 1e-5)

; fixed-point : (# -> #) # -> #
(define (fixed-point f first-guess)
  ; try : # N -> #
  (define (try guess iteration-#)
    ; close-enough? : # # -> boolean
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          (begin (display "Number of iterations: ")
                 (display iteration-#)
                 (newline)
                 next)
          (try next (+ iteration-# 1)))))
  (try first-guess 1))

; x^x = 1000
; x = log(1000) / log(x)
(define (f x) 
  (/ (log 1000) (log x)))
(define guess0 2.0)
(fixed-point f guess0) ; without average damping
(fixed-point (lambda (x) (/ (+ x (f x)) 2)) guess0) ; with average damping
