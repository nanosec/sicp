#lang racket

; zero : (X -> X) -> (X -> X)
(define zero 
  (lambda (f) 
    (lambda (x) x)))

; add-1 : ((X -> X) -> (X -> X)) -> ((X -> X) -> (X -> X))
(define (add-1 n)
  (lambda (f) 
    (lambda (x) (f ((n f) x)))))

; one : (X -> X) -> (X -> X)
(define one 
  (lambda (f)
    (lambda (x) (f x))))

; two : (X -> X) -> (X -> X)
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

; add : ((X -> X) -> (X -> X)) ((X -> X) -> (X -> X)) -> ((X -> X) -> (X -> X))
(define (add m n)
  (lambda (f)
    (lambda (x) ((n f) ((m f) x)))))

; Examples/Tests: 

(= ((zero add1) 0) 0)
(= ((one add1) 0) 1)
(= ((two add1) 0) 2)
(= (((add-1 two) add1) 0) 3)
(= (((add zero two) add1) 0) 2)
(= (((add two zero) add1) 0) 2)
(= (((add one two) add1) 0) 3)
(= (((add two two) add1) 0) 4)

(define f (lambda (x) (* x 10)))
(= ((zero f) 5) 5)
(= ((one f) 5) 50)
(= ((two f) 5) 500)
(= (((add-1 two) f) 5) 5000)
(= (((add zero two) f) 5) 500)
(= (((add two zero) f) 5) 500)
(= (((add one two) f) 5) 5000)
(= (((add two two) f) 5) 50000)