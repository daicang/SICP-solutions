(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables) (car values))
	    (make-frame (cdr variables) (cdr values)))))

(define (add-binding-to-frame! var val frame)
  (let ((new-pair (cons var val)))
    (set! frame (cons new-pair frame))))

; test ok.
;(make-frame (list 1 2 3) (list 11 22 33))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan pairs)
      (cond ((null? pairs)
	     (env-loop (enclosing-environment env)))
	    ((eq? (caar pairs) var)
	     (cadar pairs))
	    (else
	     (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan frame)))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? (caar frame) var)
	     (set-cdr! (car frame) val))
	    (else
	     (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan frame)))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan pairs)
      (cond ((null? pairs)
	     (add-binding-to-frame! var val frame))
	    ((eq? (caar pairs) var)
	     (set-cdr! (car pairs) val))
	    (else
	     (scan (cdr pairs)))))
    (scan frame)))
