(load "./structures.scm")

;;wire

(define (call-each proc-list)
  (unless (null? proc-list)
    ((car proc-list))
    (call-each (cdr proc-list))))

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

(define agenda (make-agenda))
(define or-delay 2)

(define (after-delay time proc)
  (add-agenda! agenda time proc))

(define (debug-wire name wire)
  (print "--------------------")
  (print "Name = " name)
  (print "Signal = " (get-signal wire))
  (print "--------------------"))

(define (or-gate a b out)
  (define (or-action)
    (let ((a-sig (get-signal a))
	  (b-sig (get-signal b)))
      (when (or (= 1 a-sig)
		(= 1 b-sig))
	(set-signal! out 1))))
  
  (add-action! a
	       (lambda ()
		 (after-delay or-delay or-action)))
  (add-action! b
	       (lambda ()
		 (after-delay or-delay or-action))))


(define (simple-test-or-gate)

  (define a (make-wire))
  (define b (make-wire))
  (define c (make-wire))
  (define d (make-wire))
  (define out (make-wire))

  (or-gate a b c)
  (or-gate c d out)

  (set-signal! a 1)
  (add-action! out (lambda () (debug-wire 'result out)))

  (execute-agenda! agenda))

(define nand-delay 2)

(define (logical-and sig1 sig2)
  (and (= 1 sig1)
       (= 1 sig2)))

(define (nand-gate a b out)
  (define (nand-action)
    (let ((a-sig (get-signal a))
	  (b-sig (get-signal b)))
      (if (logical-and a-sig b-sig)
	  (set-signal! out 0)
	  (set-signal! out 1))))
  (add-action! a 
	       (lambda ()
		 (after-delay nand-delay
			      nand-action)))
  (add-action! b 
	       (lambda ()
		 (after-delay nand-delay
			      nand-action))))

(define (not-gate a out)
  (nand-gate a a out))
  
;;test nand-gate
(define (simple-test-nand-gate)
  (define a (make-wire))
  (define out (make-wire))

  (not-gate a out)

  (add-action! out (lambda () (debug-wire 'result out))))


(define (and-gate a b out)
  (let ((internal-wire (make-wire)))
    (nand-gate a b internal-wire)
    (not-gate internal-wire out)))

(define (simple-test-and-gate)
  (let ((a (make-wire))
	(b (make-wire))
	(out (make-wire)))
    
    (and-gate a b out)
    (add-action! out
		 (lambda () (debug-wire 'phase-1 out)))
    (set-signal! a 1)
    (add-action! out
		 (lambda () (debug-wire 'phase-2 out)))
    (set-signal! b 1)
    (add-action! out
		 (lambda () (debug-wire 'phase-3 out)))
    (execute-agenda! agenda)))


