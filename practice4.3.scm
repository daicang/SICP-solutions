(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	(else
	 ((get 'eval (operator exp)) (operands exp) env))))
