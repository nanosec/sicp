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

(define dx 1e-5)

; newtons-method : (# -> #) # -> #
(define (newtons-method g guess)
  ; newton-transform : (# -> #) -> (# -> #)
  (define (newton-transform g)
    ; deriv : (# -> #) -> (# -> #)
    (define (deriv g)
      (lambda (x)
        (/ (- (g (+ x dx)) (g x))
           dx)))
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

; cubic : # # # -> (# -> #)
(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3)
       (* a (sqr x))
       (* b x)
       c)))

; Examples/Tests: 

; within-tolerance? : # # -> boolean
(define (within-tolerance? test-expression expected-value)
  (< (abs (- test-expression expected-value)) (* 10 tolerance)))

(within-tolerance? (newtons-method (cubic 0 0 0) 1) 0)
(within-tolerance? (newtons-method (cubic 3 3 1) 1) -1)
(within-tolerance? (newtons-method (cubic -4 -11 30) 3.5) -3)
(within-tolerance? (newtons-method (cubic -4 -11 30) 1) 2)
(within-tolerance? (newtons-method (cubic -4 -11 30) -0.5) 5)