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

; make-sum : exp exp [exp] ... [exp] -> # OR sum
(define (make-sum a1 a2 . as)
  ; make-sum-2 : exp exp -> # OR sum
  (define (make-sum-2 a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  ; make-sum-3 : exp exp (list-of exp) -> # OR sum
  (define (make-sum-3 a1 a2 as)
    (if (null? as)
        (make-sum-2 a1 a2)
        (make-sum-2 a1 (make-sum-3 a2 (car as) (cdr as)))))
  (make-sum-3 a1 a2 as))
; Examples/Tests: 
(= (make-sum 1 2) 3)
(equal? (make-sum 'x 'y) '(+ x y))
(= (make-sum 1 2 3 4) 10)
(equal? (make-sum 'x 'y 1 2) '(+ x (+ y 3)))
(equal? (make-sum 'x 1 2 'y) '(+ x (+ 1 (+ 2 y))))
(equal? (make-sum 1 2 'x 'y 'z) '(+ 1 (+ 2 (+ x (+ y z)))))

; sum? : exp -> boolean
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

; addend : sum -> exp
(define (addend s) (cadr s))
; Examples/Tests: 
(= (addend (make-sum 1 2 'x)) 1)
(same-variable? (addend (make-sum 'x 2 3)) 'x)

; augend : sum -> exp
(define (augend s) (caddr s))
; Examples/Tests: 
(equal? (augend (make-sum 1 2 'x)) '(+ 2 x))
(equal? (augend (make-sum 'x 2 3)) 5)

; make-product : exp exp -> # OR product
(define (make-product m1 m2 . ms)
  ; make-product-2 : exp exp -> # OR product
  (define (make-product-2 m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  ; make-product-3 : exp exp (list-of exp) -> # OR product
  (define (make-product-3 m1 m2 ms)
    (if (null? ms)
        (make-product-2 m1 m2)
        (make-product-2 m1 (make-product-3 m2 (car ms) (cdr ms)))))
  (make-product-3 m1 m2 ms))
; Examples/Tests: 
(= (make-product 2 3) 6)
(equal? (make-product 'x 'y) '(* x y))
(= (make-product 2 3 4 5) 120)
(equal? (make-product 'x 'y 2 3) '(* x (* y 6)))
(equal? (make-product 'x 2 3 'y) '(* x (* 2 (* 3 y))))
(equal? (make-product 2 3 'x 'y 'z) '(* 2 (* 3 (* x (* y z)))))

; product? : exp -> boolean
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; multiplier : product -> exp
(define (multiplier p) (cadr p))
; Examples/Tests: 
(= (multiplier (make-product 2 'x 3 4)) 2)
(same-variable? (multiplier (make-product 'x 2 3 4)) 'x)

; multiplicand : product -> exp
(define (multiplicand p) (caddr p))
; Examples/Tests: 
(equal? (multiplicand (make-product 2 'x 3 4)) '(* x 12))
(equal? (multiplicand (make-product 'x 2 3 4)) 24)

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
(equal-exp? (deriv (make-sum (make-product 5 'x 'y) (make-product 'x 3) 'x) 'x)
            (make-sum (make-product 5 'y) 4))
(equal-exp? (deriv (make-sum (make-exponentiation 'x 3) 'x 1) 'x)
            (make-sum (make-product 3 (make-exponentiation 'x 2)) 1))
(equal-exp? (deriv (make-product 5 'x 'y) 'y)
            (make-product 5 'x))
(equal-exp? (deriv (make-product 'x 'y (make-sum 'x 3)) 'x)
            (make-sum (make-product 'x 'y) (make-product 'y (make-sum 'x 3))))
