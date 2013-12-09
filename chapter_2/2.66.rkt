#lang racket

; A record is a list: (list key value), 
; where key is a number, and value is an X. 

; make-record : # X -> record
(define (make-record key value)
  (list key value))

; record-key : record -> #
(define (record-key record)
  (car record))

; record-value : value -> X
(define (record-value record)
  (cadr record))

; equal-record? : record record -> boolean
(define (equal-record? r1 r2)
  (and (= (record-key r1) (record-key r2))
       (equal? (record-value r1) (record-value r2))))

; A set (short for set-of-records) is either 
; 1. null, or 
; 2. (list record left right), 
; where record is a record, and left and right are trees such that
; all the records of left have keys less than the key of record, and 
; all the records of right have keys greater than the key of record. 

; make-set : record set set -> set
(define (make-set record left right)
  (list record left right))

; set-record : set -> record
(define (set-record set)
  (car set))

; set-left : set -> set
(define (set-left set)
  (cadr set))

; set-right : set -> set
(define (set-right set)
  (caddr set))

; lookup : # set -> record OR #f
(define (lookup given-key set)
  (cond ((null? set) false)
	((= given-key (record-key (set-record set)))
	 (set-record set))
	((< given-key (record-key (set-record set)))
	 (lookup given-key (set-left set)))
	(else (lookup given-key (set-right set)))))
; Examples/Tests: 
(define r1 (make-record 1 'a))
(define r2 (make-record 2 'b))
(define r3 (make-record 3 'c))
(define r4 (make-record 4 'd))
(define r5 (make-record 5 'e))
(define r6 (make-record 6 'f))

(define t1 (make-set r1 null null))
(define t3 (make-set r3 null null))
(define t2 (make-set r2 t1 t3))
(define t5 (make-set r5 null null))
(define t6 (make-set r6 t5 null))
(define t4 (make-set r4 t2 t6))

(equal-record? (lookup 1 t4) r1)
(equal-record? (lookup 2 t4) r2)
(equal-record? (lookup 3 t4) r3)
(equal-record? (lookup 4 t4) r4)
(equal-record? (lookup 5 t4) r5)
(equal-record? (lookup 6 t4) r6)
(not (lookup 0 t4))
(not (lookup 7 t4))
