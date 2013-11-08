#lang racket

; A mobile is a list of two items: (list left right), 
; where left and right are branches. 

; A branch is a list of two items: (list length structure), 
; where length is a number, and structure is either a 
; number or a mobile. 

; make-mobile : branch branch -> mobile
(define (make-mobile left right)
  (list left right))

; make-branch : # (# OR mobile) -> branch
(define (make-branch length structure)
  (list length structure))

; left-branch : mobile -> branch
(define (left-branch mobile)
  (car mobile))

; right-branch : mobile -> branch
(define (right-branch mobile)
  (car (cdr mobile)))

; branch-length : branch -> #
(define (branch-length branch)
  (car branch))

; branch-structure : branch -> # OR mobile
(define (branch-structure branch)
  (car (cdr branch)))

;; Alternative representation: 
;
;; make-mobile : branch branch -> mobile
;(define (make-mobile left right)
;  (cons left right))
;
;; make-branch : # (# OR mobile) -> branch
;(define (make-branch length structure)
;  (cons length structure))
;
;; left-branch : mobile -> branch
;(define (left-branch mobile)
;  (car mobile))
;
;; right-branch : mobile -> branch
;(define (right-branch mobile)
;  (cdr mobile))
;
;; branch-length : branch -> #
;(define (branch-length branch)
;  (car branch))
;
;; branch-structure : branch -> # OR mobile
;(define (branch-structure branch)
;  (cdr branch))

; total-weight : mobile -> #
(define (total-weight mobile0)
  ; total-weight-mobile : mobile -> #
  (define (total-weight-mobile mobile)
    (+ (total-weight-branch (left-branch mobile))
       (total-weight-branch (right-branch mobile))))
  ; total-weight-branch : branch -> #
  (define (total-weight-branch branch)
    (if (number? (branch-structure branch))
        (branch-structure branch)
        (total-weight-mobile (branch-structure branch))))
  (total-weight-mobile mobile0))
; Examples/Tests: 
(define b1 (make-branch 1 2))
(define b2 (make-branch 3 4))
(define m1 (make-mobile b1 b2))
(define b3 (make-branch 5 m1))
(define b4 (make-branch 6 7))
(define m2 (make-mobile b3 b4))
(define b5 (make-branch 8 9))
(define b6 (make-branch 10 11))
(define m3 (make-mobile b5 b6))
(define b7 (make-branch 12 m2))
(define b8 (make-branch 13 m3))
(define m4 (make-mobile b7 b8))
(= (total-weight m1) 6)
(= (total-weight m2) 13)
(= (total-weight m3) 20)
(= (total-weight m4) 33)

; balanced-mobile? : mobile -> boolean
(define (balanced-mobile? mobile0)
  ; balanced?-mobile : mobile -> boolean
  (define (balanced?-mobile mobile)
    ; torque : branch -> #
    (define (torque branch)
      (* (branch-length branch)
         (if (number? (branch-structure branch))
             (branch-structure branch)
             (total-weight (branch-structure branch)))))
    (and (= (torque (left-branch mobile))
            (torque (right-branch mobile)))
         (balanced?-branch (left-branch mobile))
         (balanced?-branch (right-branch mobile))))
  ; balanced?-branch : branch -> boolean
  (define (balanced?-branch branch)
    (if (number? (branch-structure branch))
        #t
        (balanced?-mobile (branch-structure branch))))
  (balanced?-mobile mobile0))
; Examples/Tests: 
(not (balanced-mobile? m1))
(not (balanced-mobile? m2))
(not (balanced-mobile? m3))
(define B1 (make-branch 3 4))
(define B2 (make-branch 2 6))
(define M1 (make-mobile B1 B2))
(define B3 (make-branch 2 M1))
(define B4 (make-branch 4 5))
(define M2 (make-mobile B3 B4))
(define B5 (make-branch 10 3))
(define B6 (make-branch 2 15))
(define M3 (make-mobile B5 B6))
(define B7 (make-branch 6 M2))
(define B8 (make-branch 5 M3))
(define M4 (make-branch B7 B8))
(balanced-mobile? M1)
(balanced-mobile? M2)
(balanced-mobile? M3)
(balanced-mobile? M4)
(define B2* (make-branch 1 6))
(define M1* (make-mobile B1 B2*))
(define B3* (make-branch 1 M1*))
(define M2* (make-mobile B3* B4))
(define B7* (make-branch 6 M2*))
(define M4* (make-branch B7* B8))
(not (balanced-mobile? M1*))
(not (balanced-mobile? M2*))
(not (balanced-mobile? M4*))