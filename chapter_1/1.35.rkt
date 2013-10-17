#lang racket

(define tolerance 1e-5)

; fixed-point : (# -> #) # -> #
(define (fixed-point f first-guess)
  ; try : # -> #
  (define (try guess)
    ; close-enough? : # # -> boolean
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Golden ratio
(let ((phi-approx (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
      (phi-exact (/ (+ 1 (sqrt 5)) 2)))
  (- phi-approx phi-exact))
