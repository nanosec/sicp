#lang racket

; enumerate-interval : # # -> (list-of #)
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

; accumulate : (X Y -> Y) X (list-of X) -> Y
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; flatmap : (X -> (list-of Y)) (list-of X) -> (list-of Y)
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

; unique-pairs : # -> (list-of (list N+ N+))
; to generate the sequence of pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
; Examples/Tests:
(null? (unique-pairs 0))
(null? (unique-pairs 1))
(equal? (unique-pairs 2) (list (list 2 1)))
(equal? (unique-pairs 3) (list (list 2 1) (list 3 1) (list 3 2)))

; prime? : N+ -> boolean
; to determine if n is prime
(define (prime? n)
  ; find-divisor : N+ N+ -> N+
  (define (find-divisor n test-divisor)
    ; divides? : N+ N+ -> boolean
    (define (divides? a b)
      (= (remainder b a) 0))
    (cond ((> (sqr test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define smallest-divisor (find-divisor n 2))
  (cond ((= n 1) #f)
        (else (= n smallest-divisor))))

; prime-sum-pairs : # -> (list-of (list N+ N+ N+))
(define (prime-sum-pairs n)
  ; prime-sum? : (list N+ N+) -> boolean
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  ; make-pair-sum : (list N+ N+) -> (list N+ N+ N+)
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
; Examples/Tests: 
(null? (prime-sum-pairs 1))
(equal? (prime-sum-pairs 2) (list (list 2 1 3)))
(equal? (prime-sum-pairs 3) (list (list 2 1 3) (list 3 2 5)))
(equal? (prime-sum-pairs 6)
        (list (list 2 1 3)
              (list 3 2 5)
              (list 4 1 5)
              (list 4 3 7)
              (list 5 2 7)
              (list 6 1 7)
              (list 6 5 11)))