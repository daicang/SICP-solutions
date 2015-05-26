(define (liar)
  (let ((b (amb 1 2 3 4 5))
	(e (amb 1 2 3 4 5))
	(j (amb 1 2 3 4 5))
	(k (amb 1 2 3 4 5))
	(m (amb 1 2 3 4 5)))
    (require
     (distinct? (list b e j k m)))
    (define (xor a b)
      (or (and a (not b)) (and b (not a))))
    (require (xor (= k 2) (= b 3)))
    (require (xor (= e 1) (= j 2)))
    (require (xor (= j 3) (= e 5)))
    (require (xor (= k 2) (= m 4)))
    (require (xor (= m 4) (= b 1)))
    (list ('b b)
	  ('e e)
	  ('j j)
	  ('k k)
	  ('m m))))
