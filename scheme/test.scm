(define (factoria x)
  (if (= x 1)
	1
	(* x (factoria (- x 1)))))

(define (reverse list1)
  (define (reverse-iter items result)
    (if (null? items)
      result
      (reverse-iter (cdr items) (cons (car items) result))))
    (reverse-iter list1 '()))

;(reverse (list 1 2 3 4))

(define (deep-reverse list1)
  (if (not (pair? list1))
      list1
    (reverse (map deep-reverse list1))))

;(deep-reverse (list 1 2 (list 3 4) (list 5 6)))

;(define (fringe list1)
;  (if (not (pair? list1))
;      (list list1)
;    (append (fringe (car list1)) (fringe (cdr list1)))))

(define (fringe input)
  (cond ((null? input) '())
	((not (pair? input)) (list input))
	 (else (append (fringe (car input)) (fringe (cdr input))))))

;; (list 1 2)
;; (cons 1 2)
;; (car (list 1 2))
;; (cdr (list 1 2))
;; (car (cons 1 2))
;; (cdr (cons 1 2))

(define testlist (list 1 2 (list 3 (list 4 5) 6)))
;; (cdr testlist)
;; (cdr (cdr testlist))
;; (cdr (cdr (cdr testlist)))
;; (cdr (car (cdr (cdr testlist))))
;; (fringe (list 1 2 (list 3 (list 4 5) 6)))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch struct)
  (car struct))

(define (right-branch struct)
  (car (cdr struct)))

(define (branch-length branch)
  (car branch))

(define (branch-struct branch)
  (car (cdr branch)))

(define (total-weight struct)
  ((if (not (pair? struct))
       struct
     (+ (total-weight (left-branch struct))
	(total-weight (right-branch struct))))))

;(define (is-balanced tree)
;  (if (not (pair? tree))
;      true
;    (and (= (* (total-weight (left-branch tree)) (branch-length (lef)))))))
; TODO

(define (scale-tree-1 tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree-1 (car tree) factor)
		    (scale-tree-1 (cdr tree) factor)))))

(define (scale-tree-2 tree factor)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (scale-tree-2 subtree)
	   (* subtree factor)))
       tree))

(define (subset s)
  (if (null? s)
      (list '())
    (let ((rest (subset (cdr s))))
      (append rest (map (lambda (x)
			  (cons (car s) x))
			rest)))))

; (subset (list 1 2 3 4))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
    (op (car seq)
	(accumulate op initial (cdr seq)))))

(define (filter func seq)
  (cond ((null? seq) '())
	((func (car seq))
	 (cons (car seq) (filter func (cdr seq))))
	 (else (filter func (cdr seq)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves testlist)

(define (tree-map func tree)
  (map (lambda (subtree)
       (cond ((null? subtree) '())
	     ((not (pair? subtree)) (func subtree))
	     (else (tree-map func subtree)))))
  tree)

(define (flatmap func items)
  (accumulate append '() (map func items)))

(define (enum-interval begin end)
  (enumerate-interval begin end))

(define fold-right accumulate)

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
	result
      (iter (op result (car rest))
		(cdr rest))))
  (iter initial seq))

;; (fold-right / 1 (list 1 2 3))
;; (fold-left / 1 (list 1 2 3))
;; (fold-right list '() (list 1 2 3))
;; (fold-left list '() (list 1 2 3))

(define (reverse-1 items)
  (fold-right (lambda (x y)
		(append x y))
		'() items))

(define (reverse-2 items)
  (fold-left (lambda (x y)
	       (append y x))
	     '() items))

; TODO
;(reverse-1 (list 1 2 3 4))
;(reverse-2 (list 1 2 3 4))

(define (adjoin-position x y rest-of-queens)
  (cons (list x y) rest-of-queens))

(define empty-board '())

(define (is-safe k positions)
  (let ((test (car positions))
    (former-positions (cdr positions)))
  (define (getindex index seq)
    (if (= 1 index)
	(car seq)
	(getindex (- index 1) (cdr seq))))
  (define (test-iter i former-positions)
    (if (= i k)
	true
	(if (and (queen-test-1 test (getindex i former-positions))
		 (queen-test-2 test (getindex i former-positions)))
	    (test-iter (+ 1 i) former-positions)
	    false)))
  (if (null? former-positions)
      true
      (test-iter 1 former-positions))))

(define (queen-test-1 test former)
  (not (= (car test) (car former))))

(define (queen-test-2 test former)
  (and (not (= (- (car test) (cadr test))
	       (- (car former) (cadr former))))
       (not (= (+ (car test) (cadr test))
	       (+ (car former) (cadr former))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (position)
	   (is-safe k position))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enum-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)

(define (variable? x) (symbol? x))

(define (same-variable x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum x y)
  (list '+ x y))

(define (make-product x y)
  (list '* x y))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (add-1 x) (cadr x))
(define (add-2 x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '+ (cddr x))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (mul-1 p) (cadr p))
(define (mul-2 p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (add-1) var)
		   (deriv (add-2) var)))
	((product? exp)
	 (make-sum
	  (make-product (mul-1 exp)
			(deriv (mul-2 exp) var))
	  (make-product (mul-2 exp)
			(deriv (mul-1 exp) var))))
	(else
	 (error "unknown expression type --deriv" exp))))

;; (add-2 '(+ 1 (* 2 3))) ; '(* 2 3)
;; (add-2 '(+ 1 2 3)) ; '(+ 2 3)
;; (pair? '(1)) ; true
;; (null? (cdddr '(+ 1 2))) ; true
;; (cadddr '(* x y (+ x 3)))
;(deriv '(* x y (+ x 3)) 'x) ; add-1: arity mismatch;


(define (entry tree) (car tree))
(define (left-bran tree) (cadr tree))
(define (right-bran tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (tree-list-1 tree)
  (if (null? tree)
      '()
      (append (tree-list-1 (left-bran tree))
	      (cons (entry tree)
		    (tree-list-1 (right-bran tree))))))

(define (tree-list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
	result
	(copy-to-list (left-bran tree)
		      (cons (entry tree)
			    (copy-to-list (right-bran tree) result)))))
  (copy-to-list tree '()))

(define testtree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '())
					 (make-tree 5 '() '()))
		  (make-tree 9 '() (make-tree 11 '() '()))))
(define testtree2 (make-tree 3
			     (make-tree 1 '() '())
			     (make-tree 7 (make-tree 5 '() '())
					(make-tree 9 '()
						   (make-tree 11
							      '() '())))))

;; (tree-list-1 testtree1)
;; (tree-list-2 testtree1)

;; (tree-list-1 testtree2)
;; (tree-list-2 testtree2)

;Huffman tree

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits curr)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) curr)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(leaf? (make-leaf 'C 1))
(decode sample-message sample-tree)
