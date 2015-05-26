(define (let*->nested-lets exp)
  (define (loop clauses actions)
    (if (null? clauses)
	actions
	(list 'let
	      (first-clause clauses)
	      (loop (rest-clauses clauses actions)))))
  (loop (let*-clauses exp) (let*-actions exp)))

(define (let*-clauses exp) (cadr exp))

(define (let*-actions exp) (caddr exp))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(let*->nested-lets ((x 3)
		    (y (+ x 2))
		    (z (+ x y 5)))
		   (* x z))
