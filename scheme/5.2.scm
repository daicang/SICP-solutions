
;; Register

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))

(define (set-contents! register value) ((register 'set)value))

;; Stack

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push) ;; push is function
	    ((eq? message 'pop) (pop)) ;; evaluate pop, return top
	    ((eq? message 'initialize) (initialize))
	    (else
	     (error "Unknown request -- STACK" message))))))

(define (pop stack) (stack 'pop))

(define (push stack value) ((stack 'push) value))

;; Basic machine

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ;; progarm counter
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack)))
	  (register-table
	   (list (list ('pc pc)) (list 'flag flag))))
      (define (allocate-register name)
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register: " name))))
      (define (execute) ;; TODO))))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops) ;; install library functions
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine)) ;; assemble code and run
    machine))

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list'= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
   gcd-done)))

(set-register-contents! gcd-machine 'a 206)

(set-register-contents! gcd-machine 'b 40)

(start gcd-machine)

(get-register-contents gcd-machine 'a)
