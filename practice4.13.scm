; set-car!/set-cdr! will change the value, set! will not.

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (error "Unbound variable -- MAKE-UNBOUND" var))
	    ((eq? (car vars) var)
	     (set-car! vars '())
	     (set-car! vals '()))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (scan frame)))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    ()))

(define testlist (list 1 2 3 4))

(define (test-set! arg)
  (set! arg 'test-set!))

; testlist don't change
(test-set! (cdr testlist))

(test-set! testlist)

(define (test-set-car! arg)
  (set-car! arg 'test-set-car!))

; changed
(test-set-car! testlist)

(test-set-car! (cdr testlist))
