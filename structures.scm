;;queue

(define (get-last-cdr p)
    (if (or
	 (null? p)
	 (null? (cdr p)))
	p
	(get-last-cdr (cdr p))))

(define (make-queue . vals)
  (let* ((head vals)
	 (tail (get-last-cdr head)))
    (lambda (sym)
      (define (init! val)
	(let ((new-val (list val)))
	  (set! head new-val)
	  (set! tail (get-last-cdr head))))

      (define (empty?)
	(null? head))
      
      (define (push! val)
	(if (empty?)
	    (init! val)
	    (let ((new-val (list val)))
	      (set-cdr! tail new-val)
	      (set! tail (cdr tail))))
	head)


      (define (pop!)
	(if (null? head)
	    '()
	    (let ((val (car head)))
	      (set! head (cdr head))
	      val)))
      (cond
       ((eq? sym 'pop!) pop!)
       ((eq? sym 'push!) push!)
       ((eq? sym 'empty?) empty?)
       (else (error 'definition-not-found-<queue>)))
      )))

(define (pop! queue) ((queue 'pop!)))
(define (push! queue val) ((queue 'push!) val))
(define (empty-queue? queue) ((queue 'empty?)))

;; agenda

;; time segment
(define (make-segment time . vals)
  (cons time (apply make-queue vals)))

(define (get-time segment)
  (car segment))

(define (get-segment-queue segment)
  (cdr segment))

(define (add-to-segment! segment val)
  (push! (get-segment-queue segment)
	 val))

(define (empty? segment)
  (empty-queue? (get-segment-queue segment)))

(define (execute! segment)
  (define (execute-queue! q)
    (let ((current-proc (pop! q)))
      (unless (null? current-proc)
	    (current-proc)
	    (execute-queue! q))))
  (execute-queue! (get-segment-queue segment)))

;; (time . segments)

(define (make-agenda)
  (let ((agenda (list 0)))
    (lambda (sym)
      (define (get-segments) (cdr agenda))
      (define (get-time-agenda) (car agenda))
      (define (update-time! time)
	(set-car! agenda (+ time (get-time-agenda))))
      (define (set-segments! segments)
	(set-cdr! agenda segments))

      (define (add-to-agenda! time proc)
	(define (add-to-segment-list! time proc segments)
	  (if (null? segments)
	      (list (make-segment time proc))
	      (let ((first (car segments)))
		(cond
		 ((equal? (get-time first) time)
		  (begin
		    (add-to-segment! first proc)
		    segments))
		 ((< time (get-time first))
		  (cons
		   (make-segment time proc)
		   segments))
		 (else
		  (cons first
			(add-to-segment-list!
			 time proc (cdr segments))))))))
	(set-segments!
	 (add-to-segment-list!
	  (+ time (get-time-agenda))
	  proc
	  (get-segments)))
	agenda)

      (define (remove-first!)
	(let* ((segments (get-segments))
	       (time (get-time (car segments))))
	  (update-time! time)
	  (set-cdr! agenda (cdr segments))))

      (define (execute-agenda!)
	(let ((segments (get-segments)))
	  (unless (null? segments)
	    (let ((first (car segments)))
	      (when (empty? first)
		  (remove-first!))
	      (execute! first)
	      (execute-agenda!)))))
      
      (cond
       ((eq? sym 'add-agenda!) add-to-agenda!)
       ((eq? sym 'get-time-agenda) get-time-agenda)
       ((eq? sym 'get-segments) get-segments)
       ((eq? sym 'execute-agenda!) execute-agenda!)
       (else (error 'definition-not-found-<agenda>)))
      )))

(define (add-agenda! agenda time proc)
  ((agenda 'add-agenda!) time proc))
(define (get-time-agenda agenda)
  ((agenda 'get-time-agenda)))
(define (get-segments agenda)
  ((agenda 'get-segments)))
(define (execute-agenda! agenda)
  ((agenda 'execute-agenda!)))

;; test agenda
(define (test-agenda)
  (let ((agenda (make-agenda))
	(counter 0))
    (define (update-counter!) (set! counter (+ counter 1)))

    (define (assert-counter n)
      (unless (= n counter)
	(print counter)
	(error 'assertion-failed)))

    (define (do-times n proc)
      (unless (= 0 n)
	(proc)
	(do-times (- n 1) proc)))

    (add-agenda! agenda
		 1
		 update-counter!)
    (add-agenda! agenda
		 2
		 (lambda () (assert-counter 1)))
    (add-agenda! agenda
		 2
		 (lambda () (print (get-time-agenda agenda))))
    (do-times 3
	      (lambda ()
		(add-agenda! agenda
			     3
			     update-counter!)))

    (add-agenda! agenda
		 3
		 (lambda ()
		   (add-agenda! agenda 4 update-counter!)))

    (add-agenda! agenda
		 4
		 (lambda ()
		   (print (get-time-agenda agenda))
		   (print (get-segments agenda))))

    (execute-agenda! agenda)
    (print counter)
    (print (get-segments agenda))
    ))
