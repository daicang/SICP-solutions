(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref s (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line x))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (sqrt-stream0 x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x)) (sqrt-stream0 x))))

; prematrue reference to reserved name: guessstream
; why?

;(define (sqrt-stream x)
;  (define guesses
;    (cons-stream 1.0
;		 (stream-map (lambda (guess)
;			       (sqrt-improve guess x))
;			     guesses)))
;  guesses)

(define (stream-limit s x)
  (define (check-loop prev s x)
    (if (< (abs (- prev (stream-car s))) x)
	(stream-car s)
	(check-loop (stream-car s) (stream-cdr s) x)))
  (check-loop (stream-car s) (stream-cdr s) x))

(define (ln2-stream)
  (define (ln2-stream-loop n)
    (cons-stream (/ 1.0 n)
		 (stream-map - (ln2-stream-loop (+ n 1)))))
  (ln2-stream-loop 1))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
