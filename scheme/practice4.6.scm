(define (let->combination exp)
  (append (list (list 'lambda (let-variables (let-clauses exp)) (let-body exp)))
	(let-exps (let-clauses exp))))

(define (let-exps clauses)
  (if (null? clauses)
      '()
      (cons (cadar clauses)
	    (let-exps (cdr clauses)))))

(define (let-body exp) (caddr exp))

(define (let-clauses exp) (cadr exp))

(define (let-variables clauses)
  (if (null? clauses)
      '()
      (cons (caar clauses)
	    (let-variables (cdr clauses)))))

; test
(let->combination '(let ((a 1)
			 (b 2))
		     (+ a b)))
