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

; Number of bits required: Huffman vs. fixed-length

(define pairs '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16)))
(define tree (generate-huffman-tree pairs))

(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define bits (encode message tree))

; Huffman
(length bits)  ; 84

; fixed-length
(define bits/symbol (ceiling (/ (log (length pairs)) (log 2))))
(* bits/symbol (length message))  ; 108
