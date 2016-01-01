#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? x) (eq? (car x) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (symbols x)
  (if (leaf? x)
      (list (symbol-leaf x))
      (caddr x)))

(define (weight x)
  (if (leaf? x)
      (weight-leaf x)
      (cadddr x)))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch x) (car x))

(define (right-branch x) (cadr x))

(define (choose-branch bit tree)
  (cond ((= 0 bit) (left-branch tree))
	((= 1 bit) (right-branch tree))
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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree (make-leaf 'B 2)
				  (make-code-tree (make-leaf 'D 1)
						  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

(define (in-set x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (in-set x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
	((in-set symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((in-set symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else (error "symbol not found -- ENCODE-SYMBOL" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(encode (list 'A 'B 'C 'D) sample-tree)

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
	     set
	     (let ((left (car set))
		   (right (cadr set)))
	       (successive-merge
		(adjoin-set (make-code-tree left right)
			    (cddr set))))))

(define sample-pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(generate-huffman-tree sample-pairs)
