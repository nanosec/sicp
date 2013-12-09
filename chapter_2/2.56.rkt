#lang racket

; An exp is either 
; 1. a number, 
; 2. a variable, 
; 3. a sum, 
; 4. a product, or 
; 5. an exponentiation. 

; A variable is a symbol. 

; A sum is a list: (list '+ a1 a2), where a1 and a2 are exps. 

; A product is a list: (list '* m1 m2), where m1 and m2 are exps. 

; An exponentiation is a list: (list '** base exponent), 
; where base and exponent are exps. 

; =number? : exp # -> boolean
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; variable? : exp -> boolean
(define (variable? x) (symbol? x))

; same-variable? : exp exp -> boolean
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; make-sum : exp exp -> # OR sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; sum? : exp -> boolean
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; addend : sum -> exp
(define (addend s) (cadr s))

; augend : sum -> exp
(define (augend s) (caddr s))

; make-product : exp exp -> # OR product
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; product? : exp -> boolean
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; multiplier : product -> exp
(define (multiplier p) (cadr p))

; multiplicand : product -> exp
(define (multiplicand p) (caddr p))

; make-exponentiation : exp exp -> exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
; Examples/Tests:
(= (make-exponentiation 17 0) 1)
(= (make-exponentiation (make-sum 'x 2) 0) 1)
(= (make-exponentiation 17 1) 17)
(= (make-exponentiation 2 3) 8)
(equal? (make-exponentiation (make-sum 'x 2) (make-product 'y 3))
        (list '** (make-sum 'x 2) (make-product 'y 3)))

; exponentiation? : exp -> boolean
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
; Examples/Tests: 
(exponentiation? (make-exponentiation 'x 2))
(exponentiation? (make-exponentiation (make-sum 'x 1) (make-product 'y 2)))
(not (exponentiation? 1))
(not (exponentiation? 'x))
(not (exponentiation? (make-sum 'x 1)))
(not (exponentiation? (make-product 'x 1)))
(not (exponentiation? (make-exponentiation 'x 0)))
(not (exponentiation? (make-exponentiation 'x 1)))

; base : exponentiation -> exp
(define (base e) (cadr e))
; Examples/Tests:
(= (base (make-exponentiation 2 'x)) 2)
(same-variable? (base (make-exponentiation 'x 2)) 'x)

; exponent : exponentiation -> exp
(define (exponent e) (caddr e))
; Examples/Tests: 
(= (exponent (make-exponentiation 'x 2)) 2)
(same-variable? (exponent (make-exponentiation 2 'x)) 'x)

; equal-exp? : exp exp -> boolean
(define (equal-exp? x1 x2)
  ; =sum? : exp sum -> boolean
  (define (=sum? x sum)
    (and (sum? x)
         (equal-exp? (addend x) (addend sum))
         (equal-exp? (augend x) (augend sum))))
  ; =product? : exp product -> boolean
  (define (=product? x product)
    (and (product? x)
         (equal-exp? (multiplier x) (multiplier product))
         (equal-exp? (multiplicand x) (multiplicand product))))
  ; =exponentiation? : exp exponentiation -> boolean
  (define (=exponentiation? x exponentiation)
    (and (exponentiation? x)
         (equal-exp? (base x) (base exponentiation))
         (equal-exp? (exponent x) (exponent exponentiation))))
  ; equal-exp? : exp exp -> boolean
  (define (equal-exp? x1 x2)
    (cond ((number? x1) (=number? x2 x1))
          ((variable? x1) (same-variable? x2 x1))
          ((sum? x1) (=sum? x2 x1))
          ((product? x1) (=product? x2 x1))
          ((exponentiation? x1) (=exponentiation? x2 x1))
          (else (error "unknown expression type -- EQUAL-EXP?" x1))))
  (equal-exp? x1 x2))
; Examples/Tests: 
(equal-exp? (make-sum 'x 2) (make-sum 'x 2))
(not (equal-exp? (make-sum 'x 2) (make-sum 2 'x)))
(not (equal-exp? (make-sum 'x 2) (make-sum 'x 3)))
(equal-exp? (make-product 'x 2) (make-product 'x 2))
(not (equal-exp? (make-product 'x 2) (make-product 2 'x)))
(not (equal-exp? (make-product 'x 2) (make-product 'x 3)))
(equal-exp? (make-exponentiation 'x 2) (make-exponentiation 'x 2))
(not (equal-exp? (make-exponentiation 'x 2) (make-exponentiation 2 'x)))
(not (equal-exp? (make-exponentiation 'x 2) (make-exponentiation 'x 3)))
(not (equal-exp? 'x 2))
(not (equal-exp? 2 (make-sum 'x 2)))
(not (equal-exp? (make-sum 'x 2) (make-product 'x 2)))
(not (equal-exp? (make-product 'x 2) (make-exponentiation 'x 2)))
(not (equal-exp? (make-exponentiation 'x 2) 'x))

; deriv : exp variable -> exp
; to determine the derivative of exp with respect to var
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (let ((n-1 (make-sum n -1))
                 (du/dvar (deriv u var)))
             (make-product (make-product n
                                         (make-exponentiation u n-1))
                           du/dvar))))
        (else
         (error "unknown expression type -- DERIV" exp))))
; Examples/Tests: 
(equal-exp? (deriv (make-exponentiation 2 3) 'x) 0)
(equal-exp? (deriv (make-exponentiation 'x 1) 'x) 1)
(equal-exp? (deriv (make-exponentiation 'x 3) 'x)
            (make-product 3 (make-exponentiation 'x 2)))
(define x+2y (make-sum 'x (make-product 2 'y)))
(equal-exp? (deriv (make-exponentiation x+2y 3) 'y)
            (make-product (make-product 3 (make-exponentiation x+2y 2)) 2))