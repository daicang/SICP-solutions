(define (last-exp? exp)
  (null? (cdr exp)))

(define (eval-or exp env)
  (let ((first-exp (car exp))
	(rest-exp (cdr exp))
	(first-result (eval (car exp) env)))
    (if first-result
	first-result
	(if (last-exp? exp)
	    'false
	    (eval-or rest-exp env)))))

(define (eval-and exp env)
  (let ((first-exp (car exp))
	(rest-exp (cdr exp))
	(first-result (eval (car exp) env)))
    (if first-result
	(if (last-exp? exp)
	    first-result
	    (eval-and rest-exp env))
	'false)))

(define (eval exp env)
  exp)

(eval '(1 2 3) '())
(eval-and '(1 2 3) '())
(eval-and '(1 0 1) '())

; zero-is-true
(if 0
    'zero-is-true
    'zero-is-false)
