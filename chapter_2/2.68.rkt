#lang racket

; A leaf is a list: (list 'leaf symbol weight), 
; where symbol is a symbol, and weight is a number. 

; make-leaf : symbol # -> leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; leaf? : non-empty-list -> boolean
(define (leaf? object)
  (eq? (car object) 'leaf))

; symbol-leaf : leaf -> symbol
(define (symbol-leaf x) (cadr x))

; weight-leaf : leaf -> #
(define (weight-leaf x) (caddr x))

; A code-tree is a either: 
; 1. a leaf, or 
; 2. (list left right symbols weight), 
; where left and right are code-trees, symbols 
; is a list of symbols, and weight is a number. 

; make-code-tree : code-tree code-tree -> code-tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; left-branch : code-tree -> code-tree
(define (left-branch tree) (car tree))

; right-branch : code-tree -> code-tree
(define (right-branch tree) (cadr tree))

; symbols : code-tree -> (list-of symbols)
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

; weight : code-tree -> #
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; A bit is either 0 or 1. 

; encode : (list-of symbol) code-tree -> (list-of bit)
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; encode-symbol : symbol code-tree -> (list-of bit)
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
	  null
	  (error "not in tree -- ENCODE-SYMBOL" symbol))
      (cond ((memq symbol (symbols (left-branch tree)))
	     (cons 0 (encode-symbol symbol (left-branch tree))))
	    ((memq symbol (symbols (right-branch tree)))
	     (cons 1 (encode-symbol symbol (right-branch tree))))
	    (else (error "not in tree -- ENCODE-SYMBOL" symbol)))))

; Example/Test: 
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(equal? (encode '(A D A B B C A) sample-tree) sample-message)
