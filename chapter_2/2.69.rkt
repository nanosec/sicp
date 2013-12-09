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

; A set is an ordered list with no duplicates. 

; adjoin-set : code-tree (set-of code-tree) -> (set-of code-tree)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; make-leaf-set : (list-of (cons symbol #)) -> (set-of leaf)
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; generate-huffman-tree : (list-of (cons symbol #)) -> code-tree OR null
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; successive-merge : (set-of tree) -> code-tree OR null
(define (successive-merge set-of-trees)
  (cond ((null? set-of-trees) null)
	((null? (cdr set-of-trees)) (car set-of-trees))
	(else (let ((new-tree (make-code-tree (car set-of-trees)
					      (cadr set-of-trees))))
		(successive-merge (adjoin-set new-tree (cddr set-of-trees)))))))

; Examples/Tests: 
(null? (generate-huffman-tree '()))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(equal? (generate-huffman-tree '((C 1) (D 1) (B 2) (A 4))) sample-tree)
