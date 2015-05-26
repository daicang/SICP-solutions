(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ 1 low) high))))

(define (stream-filter proc stream)
  (cond ((stream-null? stream) the-empty-stream)
	((proc (stream-car stream))
	    (cons-stream (stream-car stream)
			 (stream-filter proc (stream-cdr stream))))
	(else (stream-filter proc (stream-cdr stream)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-map1 proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
		     (stream-map1 proc (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display x)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))

(stream-ref y 7)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (devisible? x y)
  (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (devisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 100)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2)
					       factorials)))

(define (partial-sums s)
  (cons-stream (stream-car s)
	(add-streams (partioal-sums s) (stream-cdr s))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((< s2car s1car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else (cons-stream s1car
				    (merge (stream-cdr s1)
					   (stream-cdr s2))))))))

(define (my-scale-stream stream factor)
  (cons-stream (* factor (stream-car stream))
	       (scale-stream (stream-cdr stream) factor)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

(define (integrate-series exp-series)
  (define (loop-func exp n)
    (cons-stream (/ (car-stream exp) n)
		 (loop-func (cdr-stream exp) (+ n 1))))
  (loop-func exp-series 1))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x)) guesses)))
  guesses)
