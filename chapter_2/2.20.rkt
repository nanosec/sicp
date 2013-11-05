#lang racket

; same-parity : [Int] [Int] [Int] ... -> (list-of Int)
; to return a list of all the arguments to same-parity that
; have the same even-odd parity as the first argument
(define (same-parity . list-of-integers0)
  ; same-parity? : Int -> boolean
  (define (same-parity? integer)
    (boolean=? (even? integer)
               (even? (car list-of-integers0))))
  ; same-parity-aux : (list-of Int) -> (list-of Int)
  (define (same-parity-aux list-of-integers)
    (if (null? list-of-integers)
        null
        (if (same-parity? (car list-of-integers))
            (cons (car list-of-integers)
                  (same-parity-aux (cdr list-of-integers)))
            (same-parity-aux (cdr list-of-integers)))))
  (if (null? list-of-integers0)
      null
      (same-parity-aux list-of-integers0)))
; Examples/Tests: 
(equal? (same-parity) null)
(equal? (same-parity 1) (list 1))
(equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))
(equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))