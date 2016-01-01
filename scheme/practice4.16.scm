(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? (car vars) var)
	     (if (eq? (car vals) '*unassigned*)
		 (error "variable unassigned -- lookup-variable-value")
		 (car vals)))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- lookup-variable-value" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

; scribble
(define (scan-out-defines proc)
  (define (define-clauses exps)
    (cond ((null? exps) '())
	  ((eq? (caar exps) 'define)
	   (cons (car exps) (define-clauses (cdr exps))))
	  (else '())))
  (let ((clauses (define-clauses (cddr proc))))
    (let ((define-vars (map cadr clauses))
	  (define-vals (map caddr clauses))
	  (define-body (cddr proc))
	  (lambda-vars (cadr proc)))
      (list 'lambda lambda-vars
	    (cons 'let
		  (cons (map (lambda (x)
			       (list x '*unassigned*))
			     define-vars)
			(map (lambda (x)
			       (if (eq? (car x) 'define)
				   (set-car! x 'set!)
				   x))
			     define-body)))))))

(define (scan-out-defines2 proc)
  (let ((lambda-vars (cadr proc))
	(lambda-body (cddr proc)))
    (let ((define-clauses
	    (filter (lambda (x)
		      (eq? (car x) 'define))
		    lambda-body)))
      (let ((define-vars (map cadr define-clauses))
	    (define-vals (map cddr define-clauses)))
	(list 'lambda lambda-vars
	      (cons 'let
		    (cons (map (lambda (x)
				 (list x '*unassigned*))
			       define-vars)
			  (map (lambda (x)
				 (if (eq? (car x) 'define)
				     (cons 'set! (cdr xxb))
				     x))
			       lambda-body))))))))

		  
(define exp '(lambda (x)
		     (define u u1)
		     (define v v1)
		     (exp)))

; ok
(scan-out-defines2 exp)
