(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (full? items)))
  (amb (car items) (an-element-of (cdr items))))

; practice 4.35
(define (an-integer-between a b)
  (require (not (> a b)))
  (amb a (an-integer-between (+ a 1) b)))
