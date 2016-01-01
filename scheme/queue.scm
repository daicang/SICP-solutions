;; implementing queue
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
	   (set-front-ptr! q new-pair)
	   (set-rear-ptr! q new-pair)
	   q)
	  (else
	   (set-cdr! (rear-ptr q) new-pair)
	   (set-rear-ptr! q new-pair)
	   q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
	 (error "DELETE! called with an empty queue" q))
	(else
	 (set-front-ptr! q (cdr (front-ptr q)))
	 q)))
