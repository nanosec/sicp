#lang racket

; equal? : X Y -> boolean
(define (equal? x y)
  (or (and (symbol? x)
           (symbol? y)
           (eq? x y))
      (and (null? x)
           (null? y))
      (and (list? x)
           (list? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))))
; Examples/Tests:
(equal? 'a 'a)
(equal? '(a b (c d) e) '(a b (c d) e))
(not (equal? 'a 'e))
(not (equal? 'a '(a)))
(not (equal? '(a b (c d) e) '(a b c d e)))