#lang racket

(define (make-monitor f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
	    ((eq? x 'reset-count) (set! count 0))
	    (else (begin (set! count (+ count 1))
			 (f x)))))))

(define s (make-monitor sqrt))
(s 100)
(s 49)
(s 'how-many-calls?)

(define (make-accumulator x)
  (lambda (y)
    (begin (set! x (+ x y))
	   x)))

(define acc (make-accumulator 1))
(acc 2)

(define (make-account balance original-pw)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-pw msg)
    (cond ((eq? input-pw original-pw)
	   (cond ((eq? msg 'withdraw) withdraw)
		 ((eq? msg 'deposit) deposit)
		 (else (error "Unknown function" msg))))
	  (else (display "Incorrect password"))))
  dispatch)

(define account1 (make-account 100 'passwd))
((account1 'passwd 'withdraw) 50) ; 50
;((account1 'wrong-passwd 'deposit) 100)

(define (make-account1 balance ori-pw)
  (let ((pw-err-cnt 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops) (error "call-the-cops"))
    (define (dispatch input-pw func)
      (if (eq? input-pw ori-pw)
	  (cond ((eq? func 'withdraw) withdraw)
		((eq? func 'deposit) deposit)
		(else (error "Unknown function")))
	  (begin (set! pw-err-cnt (+ 1 pw-err-cnt))
		 (if (> pw-err-cnt 7)
		     (call-the-cops)
		     "Wrong passwd"))))
    dispatch))

(define account2 (make-account1 100 'passwd))
((account2 'passwd 'withdraw) 50)
((account2 'passwd 'withdraw) 80)
((account2 'passwd 'deposit) 200)
;((account2 'wrong-pw 'deposit) 30)

(define (make-joint acc ori-pw new-pw)
  (lambda (input-pw func)
    (if (eq? new-pw input-pw)
	(acc ori-pw func)
	(error "Wrong password"))))

(define account3 (make-joint account2 'passwd 'new-passwd))
((account3 'new-passwd 'deposit) 100)
;((account3 'passwd 'withdraw) 100)

(define (f x)
  (let ((call-count 0)
	(return-val 0))
    (begin (+ call-count 1)
	   (if (= call-count 1)
	       (if (= x 0)
		   (set! return-val 0)
		   (set! return-val 0.5))
	       '())
	   return-val)))

(+ (f 0) (f 1))

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(require scheme/mpair)
(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
(mcdr x)
(define w (append! x y))
(mcdr x)

(define (assoc-old key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc-old key (cdr records)))))

(define (make-table-old)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-old key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc-old key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-old key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc-old key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))