(define (all-different l)
  (define (different first rest)
    (if (null? rest)
	#t
	(if (= first (car rest))
	    #f
	    (different first (cdr rest)))))
  (if (null? l)
      #t
      (if (different (car l) (cdr l))
	  (all-different (cdr l))
	  #f)))

(define (extend-list l)
  (define (not-appear num)
    (define (loop num l)
      (if (null? l)
	  #t
	  (if (= num (car l))
	      #f
	      (loop num (cdr l)))))
    (loop num l))
  (let ((numbers '(1 2 3 4 5)))
    (let ((valid-numbers
	   (filter
	    not-appear
	    numbers)))
      (map (lambda (x)
	     (append l (list x)))
	   valid-numbers))))

(define (accumulate op proc x)
  (if (null? x)
      '()
      (op (proc (car x))
	  (accumulate op proc (cdr x)))))

(define (flatmap proc x)
  (accumulate append proc x))

(define (solve)
  (define (r-all x)
    (and (not (= (car x) 5))
	 (not (= (cadr x) 1))
	 (not (or (= (caddr x) 1) (= (caddr x) 5)))
	 (= (- (cadddr x) (caddr x)) 1)
	 (not (= (abs (- (car (cddddr x)) (caddr x))) 1))
	 (not (= (abs (- (caddr x) (cadr x))) 1))))
  (filter
   r-all
   (filter
    all-different
    (flatmap extend-list
	     (flatmap extend-list
		      (flatmap extend-list
			       (flatmap extend-list
					'((1) (2) (3) (4) (5)))))))))
  
