(load "queue.scm")

(define (call-each action-list)
  (if (null? action-list)
      'done
      (begin
	((car action-list))
	(call-each (cdr action-list)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation -- WIRE" m))))
    dispatch))


(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1))
      1
      0))

(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1))
      1
      0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-proc)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

(define (new-or-gate a1 a2 output)
  (let ((wire-1 (make-wire))
	(wire-2 (make-wire))
	(wire-3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output))
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder lista listb lists c-wire)
  (define (iter a b s c-signal)
    (if (null? a)
	'ok
	(let ((c-in (make-wire))
	      (c-out (make-wire)))
	  (set-signal! c-in c-signal)
	  (full-adder (car a) (car b) c-in (car s) c-out)
	  (iter (cdr a) (cdr b) (cdr s) (get-signal c-out)))))
  (iter lista listb lists (get-signal c-wire)))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-val)
  ((wire 'set-signal!) new-val))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))

(define (make-agenda)
  (list 0))

(define (current-time agenda)
  (car agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ (current-time the-agenda) delay) action the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display "name = ")
		 (display name)
		 (newline)
		 (display "curren time = ")
		 (display (current-time the-agenda))
		 (newline)
		 (display "new-value = ")
		 (display (get-signal wire))
		 (newline))))

;; data difinition
(define the-agenda (make-agenda))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; test probe
(probe 'sum sum)
(probe 'carry carry)

;; implementing agenda
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

;; add-to-agenda: core function
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(insert-queue! (segment-queue (car segments)) action)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr! segments
			(cons (make-new-time-segment time action)
			      (cdr segments)))
	      (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments! agenda (cons (make-new-time-segment time action)
			       segments))
	(add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM" agenda)
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))
;; test propagate
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
