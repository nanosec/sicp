#lang racket

; cc : # (list-of #) -> #
; to count the number of ways to create change adding up to amount
; using coins with values listed in coin-values
(define (cc amount coin-values)
  ; no-more? : (list-of #) -> boolean
  (define (no-more? coin-values)
    (null? coin-values))
  ; first-denomination : (list-of #) -> #
  (define (first-denomination coin-values)
    (car coin-values))
  ; except-first-denomination : (list-of #) -> (list-of #)
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
; Examples/Tests: 
(define us-coins (list 50 25 10 5 1))
(= (cc -1 us-coins) 0)
(= (cc 0 us-coins) 1)
(= (cc 10 us-coins) 4)
(= (cc 100 us-coins) 292)
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(= (cc 2 uk-coins) 4)
(= (cc 4 uk-coins) 9)