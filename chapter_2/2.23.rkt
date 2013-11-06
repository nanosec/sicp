#lang racket

; for-each : (X -> Y) (list-of X) -> #t
; to apply procedure on each element of a-list0, 
; and to return #t when done
(define (for-each procedure a-list0)
  ; for-each-aux : (list-of X) Y -> #t
  (define (for-each-aux a-list procedure-application)
    (if (null? a-list)
        #t
        (for-each-aux (cdr a-list)
                      (procedure (car a-list)))))
  (for-each-aux a-list0 #f))
; Examples/Tests: 
(for-each display null)
(for-each display (list 1 2 3))
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))