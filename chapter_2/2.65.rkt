#lang racket

; A tree is either 
; 1. null, or 
; 2. (list entry left right), 
; where entry is a number, and left and right are trees
; such that all elements of left are less than entry, 
; and all elements of right are greater than entry. 

; make-tree : X tree tree -> tree
(define (make-tree entry left right)
  (list entry left right))

; entry : tree -> X
(define (entry tree) (car tree))

; left-branch : tree -> tree
(define (left-branch tree) (cadr tree))

; right-branch : tree -> tree
(define (right-branch tree) (caddr tree))

; tree->list : tree -> list
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; list->tree : list -> tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; partial-tree : list N -> (cons tree list)
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; A set is a tree. 

; element-of-set? : # set -> boolean
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

; adjoin-set : # set -> set
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; intersection-set : set set -> set
(define (intersection-set set1 set2)
  ; intersection-list : list list -> list
  ; Assumption: list1 and list2 are ordered
  (define (intersection-list list1 list2)
    (cond ((null? list1) null)
	  ((null? list2) null)
	  (else (cond ((< (car list1) (car list2))
		       (intersection-list (cdr list1) list2))
		      ((= (car list1) (car list2))
		       (cons (car list1)
			     (intersection-list (cdr list1) (cdr list2))))
		      (else (intersection-list list1 (cdr list2)))))))
  (let ((list1 (tree->list set1))
	(list2 (tree->list set2)))
    (list->tree (intersection-list list1 list2))))
; Examples/Tests: 
(null? (intersection-set null (list->tree '(1 2 3))))
(null? (intersection-set (list->tree '(1 2 3)) null))
(null? (intersection-set (list->tree '(1 2 3)) (list->tree '(4 5 6))))
(equal? (intersection-set (list->tree '(1 3 5)) (list->tree '(2 3 4 5 6)))
	(list->tree '(3 5)))
(equal? (intersection-set (list->tree '(1 2 3 4)) (list->tree '(0 1 3 4)))
	(list->tree '(1 3 4)))

; union-set : set set -> set
(define (union-set set1 set2)
  ; union-list : list list -> list
  ; Assumption: list1 and list2 are ordered
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
	  ((null? list2) list1)
	  (else (cond ((< (car list1) (car list2))
		       (cons (car list1)
			     (union-list (cdr list1) list2)))
		      ((= (car list1) (car list2))
		       (cons (car list1)
			     (union-list (cdr list1) (cdr list2))))
		      (else (cons (car list2)
				  (union-list list1 (cdr list2))))))))
  (let ((list1 (tree->list set1))
	(list2 (tree->list set2)))
    (list->tree (union-list list1 list2))))
; Example/Tests: 
(null? (union-set null null))
(equal? (union-set (list->tree '(1 2 3)) null)
	(list->tree '(1 2 3)))
(equal? (union-set (list->tree '(1 2 3)) (list->tree '(4 5 6)))
	(list->tree '(1 2 3 4 5 6)))
(equal? (union-set (list->tree '(1 3 5)) (list->tree '(2 3 4 5 6)))
	(list->tree '(1 2 3 4 5 6)))
(equal? (union-set (list->tree '(1 2 3 4)) (list->tree '(0 1 3 4)))
	(list->tree '(0 1 2 3 4)))
