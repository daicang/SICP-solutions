(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (last-seq? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (make-if predicate consequence alternative)
  (list 'if predicate consequence alternative))

(define (new-cond? clause) (eq? '=> (cadr clause)))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (new-cond-proc clause) (caddr clause))

(define (new-cond-args clause) (car clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(if (new-cond? first)
		    ((new-cond-proc first) (new-cond-args first))
		    (sequence->exp (cond-actions first)))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate clause)
		     (if (new-cond? first)
			 ((new-cond-proc first) (new-cond-args first))
			 (sequence->exp (cond-actions first)))
		     (expand-clauses rest))))))
