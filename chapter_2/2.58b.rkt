#lang racket

; An exp is either 
; 1. a simple-exp, or 
; 2. a compound-exp. 

; A simple-exp is either 
; 1. a number, or 
; 2. a variable. 

; A variable is a symbol. 

; A compound-exp is a list: (list e1 op e2), 
; where e1 and e2 are exps, and op is an operator. 

; An operator is either '+, '*, or '**. 

; A sum is a compound-exp where the operator is '+. 
; A product is a compound-exp where the operator is '*. 
; An exponentiation is a compound-exp where the operator is '**. 

; =number? : exp # -> boolean
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; variable? : exp -> boolean
(define (variable? x) (symbol? x))

; same-variable? : exp exp -> boolean
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; proper-exp : (list simple-exp) OR compound-exp -> exp
(define (proper-exp x)
  (if (null? (cdr x))
      (car x)
      x))
; Examples/Tests: 
(= (proper-exp '(1)) 1)
(same-variable? (proper-exp '(x)) 'x)
(equal? (proper-exp '(x + y)) '(x + y))
(equal? (proper-exp '(x * y)) '(x * y))
(equal? (proper-exp '(x ** y)) '(x ** y))

; make-sum : exp exp -> # OR sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

; sum? : exp -> boolean
(define (sum? x)
  ; sum?-compound : compound-exp OR (list simple-exp) -> boolean
  (define (sum?-compound x)
    (cond ((null? (cdr x)) #f)
	  ((eq? (cadr x) '+) #t)
	  (else (sum?-compound (cddr x)))))
  (and (pair? x) (sum?-compound x)))
; Examples/Tests: 
(sum? '(2 * x + 4 ** y))
(sum? '(a ** b * c ** d + e))
(not (sum? 1))
(not (sum? 'x))
(not (sum? '(a * b)))
(not (sum? '(a ** b)))
(not (sum? '((x + 1) * 4 ** y)))

; addend : sum -> exp
(define (addend s)
  ; addend-aux : sum -> (list simple-exp) OR compound-exp
  (define (addend-aux s)
    (if (eq? (cadr s) '+)
	(list (car s))
	(cons (car s) (cons (cadr s) (addend-aux (cddr s))))))
  (proper-exp (addend-aux s)))
; Examples/Tests: 
(= (addend '(3 + x)) 3)
(same-variable? (addend '(x + 3)) 'x)
(equal? (addend '(2 * 3 + 4 * 5)) '(2 * 3))
(equal? (addend '((a + b) ** c * d + e + f)) '((a + b) ** c * d))

; augend : sum -> exp
(define (augend s)
  (let ((result (cdr (memq '+ s))))
    (proper-exp result)))
; Examples/Tests: 
(= (augend '(x + 3)) 3)
(same-variable? (augend '(3 + x)) 'x)
(equal? (augend '(2 * 3 + 4 * 5)) '(4 * 5))
(equal? (augend '((a + b) ** c * d + e + f)) '(e + f))

; make-product : exp exp -> # OR product
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; product? : exp -> boolean
(define (product? x)
  ; product?-compound : (compound-exp OR (list simple-exp)) boolean -> boolean
  (define (product?-compound x encountered-*?)
    (cond ((null? (cdr x)) encountered-*?)
	  ((eq? (cadr x) '+) #f)
	  ((eq? (cadr x) '*) (product?-compound (cddr x) #t))
	  (else (product?-compound (cddr x) encountered-*?))))
  (and (pair? x) (product?-compound x #f)))
; Examples/Tests: 
(product? '(a * b))
(product? '((a + b) ** c ** d * e))
(not (product? 6))
(not (product? 'x))
(not (product? '(a + b)))
(not (product? '(a ** b)))
(not (product? '(a * b ** c + d * e)))

; multiplier : product -> exp
(define (multiplier p)
  ; multiplier-aux : product -> (list simple-exp) OR compound-exp
  (define (multiplier-aux p)
    (if (eq? (cadr p) '*)
	(list (car p))
	(cons (car p) (cons (cadr p) (multiplier-aux (cddr p))))))
  (proper-exp (multiplier-aux p)))
; Examples/Tests: 
(= (multiplier '(2 * x)) 2)
(same-variable? (multiplier '(x * 2)) 'x)
(equal? (multiplier '((a + b) * c * d)) '(a + b))
(equal? (multiplier '(a ** b ** c ** d * e ** f)) '(a ** b ** c ** d))

; multiplicand : product -> exp
(define (multiplicand p)
  (let ((result (cdr (memq '* p))))
    (proper-exp result)))
; Examples/Tests: 
(= (multiplicand '(x * 2)) 2)
(same-variable? (multiplicand '(2 * x)) 'x)
(equal? (multiplicand '((a + b) * c * d)) '(c * d))
(equal? (multiplicand '(a ** b ** c ** d * e ** f)) '(e ** f))

; make-exponentiation : exp exp -> exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list base '** exponent))))

; exponentiation? : exp -> boolean
(define (exponentiation? x)
  ; exponentiation?-compound : (compound-exp OR (list simple-exp)) bool -> bool
  (define (exponentiation?-compound x encountered-**?)
    (cond ((null? (cdr x)) encountered-**?)
	  ((or (eq? (cadr x) '+)
	       (eq? (cadr x) '*))
	   #f)
	  ((eq? (cadr x) '**) (exponentiation?-compound (cddr x) #t))
	  (else (exponentiation?-compound (cddr x) encountered-**?))))
  (and (pair? x) (exponentiation?-compound x #f)))
; Examples/Tests: 
(exponentiation? '(a ** b))
(exponentiation? '((a * b) ** (c + d)))
(not (exponentiation? 6))
(not (exponentiation? 'x))
(not (exponentiation? '(a + b)))
(not (exponentiation? '(a * b)))
(not (exponentiation? '(a ** b + c)))
(not (exponentiation? '((a ** b) ** c ** d ** e * f)))

; base : exponentiation -> exp
(define (base e) (car e))

; exponent : exponentiation -> exp
(define (exponent e)
  (let ((result (cddr e)))
    (proper-exp result)))
; Examples/Tests: 
(= (exponent '(x ** 2)) 2)
(same-variable? (exponent '(2 ** x)) 'x)
(equal? (exponent '(a ** b ** c ** d)) '(b ** c ** d))
(equal? (exponent '((a ** b) ** c ** d)) '(c ** d))

; equal-exp? : exp exp -> boolean
(define (equal-exp? x1 x2)
  ; equal-exp? : exp exp -> boolean
  (define (equal-exp? x1 x2)
    (cond ((number? x1) (=number? x2 x1))
          ((variable? x1) (same-variable? x2 x1))
          ((sum? x1) (=sum? x2 x1))
          ((product? x1) (=product? x2 x1))
          ((exponentiation? x1) (=exponentiation? x2 x1))
          (else (error "unknown expression type -- EQUAL-EXP?" x1))))
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
(equal-exp? (deriv '(x + 3 * (x + y + 2)) 'x) 4)
(equal-exp? (deriv '(a * x ** (2 * x) * b + x * (c + d)) 'x)
	    '((a * (((2 * x) * (x ** ((2 * x) + -1))) * b)) + (c + d)))
(equal-exp? (deriv '((x * 3 + a) * (b * x ** 2 + c) + x * d + e) 'x)
	    '((((x * 3 + a) * (b * (2 * x))) + (3 * (b * x ** 2 + c))) + d))
(equal-exp? (deriv '(a * x + x * b ** c + d * x ** 3 * e + x + f) 'x)
	    '(a + ((b ** c) + ((d * ((3 * (x ** 2)) * e)) + 1))))
(equal-exp? (deriv '((a + b) * x + y * x ** a ** 2 + x * b ** 3) 'x)
	    '((a + b) + ((y * ((a ** 2) * (x ** ((a ** 2) + -1)))) + (b ** 3))))
