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

; decode : (list-of bit) code-tree -> (list-of symbol)
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; choose-branch : bit code-tree -> code-tree
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Example/Test: 
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(equal? (decode sample-message sample-tree) '(A D A B B C A))
