;;todo redesign queue in a simple manner
;;todo refactoring the agenda definition



(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (queue . args)
  (if (null? args) (list 'queue-t)
      (cons 'queue-t (cons args
			   (last-pair args)))))

(define (queue? q)
  (and
   (not (atom? q))
   (eq? 'queue-t (car q))))

(define (push! q value)
  (if (queue? q)
      (let ((lst (list value)))
	(if (null? (cdr q))
	    (set-cdr! q
		      (cons lst lst))
	    (begin
	      (set-cdr! (cddr q) lst)
	      (set-cdr! (cdr q) lst)))
	
	q)
      (error 'not-a-queue)))

(define (pop! q)
  (if (queue? q)
      (let ((lst (cadr q)))
	(if (null? (cdr lst))
	    (set-cdr! q '())
	    (set-car! (cdr q)
		      (cdr lst)))
	(car lst))
      (error 'not-a-queue)))

(define (first-in-queue q)
  (if (queue? q)
      (caadr q)
      (error 'not-a-queue)))

(define (last-in-queue q)
  (if (queue? q)
      (caddr q)
      (error 'not-a-queue)))




;;logical gate simulator
(define (call-each proc-list)
  (if (null? proc-list) 'ok
      (begin
	((car proc-list))
	(call-each (cdr proc-list)))))

(define (make-wire)
  (let ((signal 0)
	(proc-list '()))

    (define (get-signal) signal)
    (define (set-signal! new-sig)
      (unless (equal? signal new-sig)
	(begin
	  (set! signal new-sig)
	  (call-each proc-list))))
    (define (add-action! proc)
      (set! proc-list (cons proc proc-list))
      (proc))

    (define (dispatch sym)
      (cond
       ((eq? sym 'set-signal!) set-signal!)
       ((eq? sym 'add-action!) add-action!)
       ((eq? sym 'get-signal) get-signal)
       (else
	(error 'definition-not-found))))

    dispatch))

(define (get-signal wire) ((wire 'get-signal)))
(define (set-signal! wire new-signal) ((wire 'set-signal!) new-signal))
(define (add-action! wire proc) ((wire 'add-action!) proc))

(define (make-agenda) (list 0))
(define (make-time-segment time proc) (cons time (queue proc)))
(define (get-time segment) (car segment))
(define (get-queue segment) (cdr segment))
(define (add-time! agenda time)
  (set-car! agenda (+ (get-time agenda) time)))
(define (get-segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (empty-agenda? agenda)
  (null? (get-segments agenda)))
(define (get-first-segment agenda) (car (get-segments agenda)))


(define (add-to-agenda! agenda time proc)
  (define (add-step! new-segment segments)
    (let ((first (car segments)))
      (cond
       ((equal? (get-time first) time)
	(push! (get-queue first) proc))
       ((< time (get-time first))
	(begin
	  (set-cdr! segments
		    (cons first
			  (cdr segments)))
	  (set-car! segments new-segment)))
       ((null? (cdr segments))
	(set-cdr! segments new-segment))
       (else
	(add-step! new-segment (cdr segments))))))
  
  (if (null? (get-segments agenda))
      (set-segments! agenda (make-time-segment time proc))
      (add-step! (make-time-segment time proc) (get-segments agenda))))

;; add-action! adds the proc to the proc list and executes it immediatly after
;; this because all wires related actions are defined as after-delay, which
;; add a record to the agenda when it is executed.
;; In fact, the agenda is the core of the simulation through the propagate
;; method which executes all the lambdas added via after-delay.

;; The set-signal! wires' functioni set the new signal and call
;; each procedure inside the proc list, which in turn should add
;; new records to the agenda to be executed.
;; This even-driven structure is like a cascade mechanic.
;; A definition of queue is required.

(define agenda (make-agenda))

(define (execute-first! agenda)
  (let ((segments (get-segments agenda)))
    (if (null? segments) 'empty-agenda
	(let ((first (get-first-segment agenda)))
	  (add-time! agenda (get-time first))
	  ((pop! (get-queue first)))
	  (when (null? (get-queue first))
	    (set-cdr! agenda (cdr segments)))))))

(define (propagate)
  (unless (empty-agenda? agenda)
    (begin
      (execute-first! agenda)
      (propagate))))


(define (after-delay delay proc)
  (add-to-agenda! agenda
		  delay
		  proc)
  (proc))

(define or-delay 1)
(define (logical-or a b)
  (if (or (get-signal a)
	  (get-signal b))
      1
      0))

(define (or-gate a b out)
  (define (or-action)
    (let ((new-sig
	   (logical-or a b)))
      (after-delay or-delay
		   (lambda ()
		     (set-signal! out new-sig)))))
  (add-action! a or-action)
  (add-action! b or-action)) 
