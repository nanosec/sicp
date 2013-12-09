#lang racket

; An exp is either 
; 1. a number, 
; 2. a variable, 
; 3. a sum, 
; 4. a product, or 
; 5. an exponentiation. 

; A variable is a symbol. 

; A sum is a list: (list '+ a1 ... an), where a1 ... an are exps. 

; A product is a list: (list '* m1 ... mn), where m1 ... mn are exps. 

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
(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '+ (cddr x))))
; Examples/Tests: 
(= (augend '(+ x 1)) 1)
(same-variable? (augend '(+ 1 x)) 'x)
(equal? (augend '(+ x 1 2)) '(+ 1 2))
(equal? (augend '(+ a b c d)) '(+ b c d))

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
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))
; Examples/Tests: 
(= (multiplicand '(* x 2)) 2)
(same-variable? (multiplicand '(* 2 x)) 'x)
(equal? (multiplicand '(* x 2 3)) '(* 2 3))
(equal? (multiplicand '(* a b c d)) '(* b c d))

; make-exponentiation : exp exp -> exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

; exponentiation? : exp -> boolean
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

; base : exponentiation -> exp
(define (base e) (cadr e))

; exponent : exponentiation -> exp
(define (exponent e) (caddr e))

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
(equal-exp? (deriv '(+ (* 5 x y) (* x 3) x) 'x)
	    '(+ (* 5 y) 4))
(equal-exp? (deriv '(+ (** x 3) x 1) 'x)
	    '(+ (* 3 (** x 2)) 1))
(equal-exp? (deriv '(* 5 x y) 'y)
	    '(* 5 x))
(equal-exp? (deriv '(* x y (+ x 3)) 'x)
	    '(+ (* x y) (* y (+ x 3))))
