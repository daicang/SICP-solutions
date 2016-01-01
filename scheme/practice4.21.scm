((lambda (n)
  ((lambda (f)
     (f f n))
   (lambda (ft k)
     (if (= k 1)
	 1
	 (* k (ft ft (- k 1))))))) 10)

((lambda (n)
   ((lambda (f)
      (f f n))
    (lambda (f k)
      (if (or (= k 1) (= k 2))
	  1
	  (+ (f f (- k 1)) (f f (- k 2))))))) 7)
