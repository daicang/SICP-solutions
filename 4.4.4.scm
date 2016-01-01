;; Use "dc-force" instead of "force", "dc-delay" instead of "delay".

;; Stream operations
;(define the-empty-stream '())
(define (stream-null? s)
  (if (null? s)
      #t
      #f))


;; Implement delay and force: page 225
(define (dc-delay s)
  (lambda () s))

(define (dc-force delayed-obj)
  (delayed-obj))

;; Implement stream by delay and force
(define (stream-car s) (car s))
(define (stream-cdr s) (dc-force (cdr stream)))

;; Page 223
(define (cons-stream s1 s2)
  (cons s1 (dc-delay s2)))

;; Page 222
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-filter p s)
  (cond ((stream-null? s) the-empty-stream)
	((p (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter p (stream-cdr s))))
	(else (stream-filter p (stream-cdr s)))))

;; Query system
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
	   (let ((binding (binding-in-frame exp frame)))
	     (if binding
		 ))))))

(define (query-driver-loop)
  (prompt-for-input input)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
	   (add-rule-or-assertion! (add-assertion-body q))
	   (newline)
	   (display "Assertion added to data base.")
	   (query-driver-loop))
	  (else
	   (newline)
	   (display output-prompt)
	   (display-stream
	    (stream-map
	     (lambda (frame)
	       (instantiate q
			    frame
			    (lambda (v f)
			      (contract-question-mark v))))
	     (qeval q (singleton-stream '()))))
	   (query-driver-loop)))))
