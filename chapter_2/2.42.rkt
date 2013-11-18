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

; A position is a pair: (cons row column), 
; where row and column are natural numbers. 

; make-position : N N -> position
(define (make-position row column)
  (cons row column))

; position-row : position -> N
(define (position-row position)
  (car position))

; position-column : position -> N
(define (position-column position)
  (cdr position))

; A board is a (list-of position). 

; empty-board : board
(define empty-board null)

; adjoin-position : N N board -> board
(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

; safe? : N board -> boolean
; to determine if the new queen in the kth column of a board is safe, 
; assuming that the queens in the first k-1 columns were all safe
; Note: positions should be (cons new-queen rest-of-queens)
(define (safe? k positions)
  ; safe-positions? : position position -> boolean
  (define (safe-positions? p1 p2)
    ; same-row? : position position -> boolean
    (define (same-row? p1 p2)
      (= (position-row p1) (position-row p2)))
    ; same-column? : position position -> boolean
    (define (same-column? p1 p2)
      (= (position-column p1) (position-column p2)))
    ; same-diagonal? : position position -> boolean
    (define (same-diagonal? p1 p2)
      (let ((slope (/ (- (position-row p1) (position-row p2))
                      (- (position-column p1) (position-column p2)))))
        (= (abs slope) 1)))
    (not (or (same-row? p1 p2)
             (same-column? p1 p2)
             (same-diagonal? p1 p2))))
  (let ((new-queen (car positions))
        (rest-of-queens (cdr positions)))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda (queen) (safe-positions? queen new-queen))
                     rest-of-queens))))

; queens : N -> (list-of board)
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Examples/Tests: 
(equal? (queens 0) (list empty-board))
(equal? (queens 1) (list (list (make-position 1 1))))
(null? (queens 2))
(null? (queens 3))
(equal? (queens 4) (list (list (make-position 3 4)
                               (make-position 1 3)
                               (make-position 4 2)
                               (make-position 2 1))
                         (list (make-position 2 4)
                               (make-position 4 3)
                               (make-position 1 2)
                               (make-position 3 1))))
(= (length (queens 8)) 92)