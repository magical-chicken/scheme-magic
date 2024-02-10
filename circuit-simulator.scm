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
  (print "--------------------")
  )

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


;;todo move all tests in a separate file
;;todo put an ascii illustration here!

(define (1bit-memory i s o)
  (let ((a (make-wire))
	(b (make-wire))
	(c (make-wire)))
    (nand-gate i s a)
    (nand-gate a s b)
    (nand-gate a c o)
    (nand-gate o b c)))

;;todo test
;;todo documentation
;;todo better args naming

(define (make-wires n)
  (if (< n 1)
      '()
      (cons (make-wire)
	    (make-wires (- n 1)))))

(define (debug-wires name wires)
  (for-each
   (lambda (wire) (add-action! wire
			  (lambda () (debug-wire name wire))))
   wires))

;;todo error-checkin in set-signal for wire
(define (set-signals-wires! wires signals)
  (for-each
   (lambda (wire bit)
     (when (or (> bit 1)
	       (< bit 0))
       (error 'invalid-bit-value))
     (set-signal! wire bit))
   wires
   signals))

(define (wires-signals wires)
  (map get-signal wires))

(define (nbyte-memory i o s)
  (for-each (lambda (a b)
	      (1bit-memory a s b))
	    i o))

(define (test-nmemory)
  (let ((a (make-wires 8))
	(b (make-wires 8))
	(set (make-wire)))

    (set-signals-wires! a (list 1 1 1 0 1 1 0 0))
    (debug-wires 'input a)
    (debug-wires 'output b)
    (nbyte-memory a b set)
    (after-delay 3 (lambda ()
		     (set-signal! set 1)))
    (execute-agenda! agenda)
    (wires-signals b)
    ))


(define (enabler i o e)
  (for-each (lambda (a b)
	 (and-gate a e b))
	    i o))

(define (test-enabler)
  (let ((a (make-wires 8))
	(b (make-wires 8))
	(enab (make-wire)))
    
    (set-signals-wires! a (list 1 1 0 1 0 1 1 0))
    (debug-wires 'input a)
    (debug-wires 'output b)

    (add-action! enab (lambda () (debug-wire 'enable enab)))
    (enabler a b enab)
    
    (define (enabling-output)
          (after-delay 3
		   (lambda ()
		     (set-signal! enab 1)))
      
      (execute-agenda! agenda)
      (print (wires-signals b)))

    (define (disabling-output)
      (after-delay 3
		   (lambda ()
		     (set-signal! enab 1)))

      (after-delay 3
		   (lambda ()
		     (set-signal! enab 0)))
      
      (execute-agenda! agenda)
      (print (wires-signals b)))
    (enabling-output)
    (disabling-output)
  ))
	

(define (register i o set enable bits-num)
  (let ((iwires (make-wires bits-num)))
    (nbyte-memory i iwires set)
    (enabler iwires o enable)))

(define (test-memory)
  (let ((i (make-wire))
	(s (make-wire))
	(o (make-wire)))

    (1bit-memory i s o)

    ;; starting with 1
    (set-signal! i 1)
    (after-delay 1
		 (lambda () (set-signal! s 1)))
    (after-delay 1
		 (lambda () (set-signal! s 0)))
    (after-delay 1
		 (lambda () (set-signal! i 0)))
    (add-action! i (lambda () (debug-wire 'input i)))
    (add-action! o (lambda () (debug-wire 'output o)))
    (add-action! s (lambda () (debug-wire 'setter s)))

    (execute-agenda! agenda)
    ))

;; todo substitute set-signal! with on! and off!
;; todo a better debug system is needed

(define (test-register-8bit)
  (let ((input (make-wires 8))
	(out (make-wires 8))
	(set (make-wire))
	(enable (make-wire))) 

    (set-signals-wires! input (list 1 0 1 0 1 1 0 0))
    (debug-wires 'INPUT input)
    (debug-wires 'OUTPUT out)
    
    ;; making one register
    (register input out set enable 8)
    (add-action! set (lambda () (debug-wire 'setter set)))
    (set-signal! set 1)
    (add-action! enable (lambda () (debug-wire 'enable enable)))

    ;; if enabler 0 out if totally off.
    ;; turning off the set signal to see if
    ;; register saves the value.
    
    (after-delay 3
		 (lambda () (set-signal! set 0)))

    ;; activating enable to see the output
    (after-delay 4
		 (lambda () (set-signal! enable 1)))

    (after-delay 4
		 (lambda () (set-signal! enable 0)))

    (after-delay 4
		 (lambda () (set-signals-wires! input (list 0 0 0 0 0 0 0))))

    (after-delay 6
		 (lambda () (set-signal! enable 1)))
    

    (execute-agenda! agenda)
    (print (wires-signals out))
    
    ))

(define (get-wire ind wires)
  (cond
   ((null? wires) '())
   ((= 0 ind) (car wires))
   (else (get-wire (- ind 1) (cdr wires)))))

(define (tree-wire-and-gate a b c out)
  (let ((iwire (make-wire)))
    (and-gate a b iwire)
    (and-gate iwire c out)))

(define (decoder a b c out)
  (let ((iwire-a (make-wire))
	(iwire-b (make-wire))
	(iwire-c (make-wire)))
    (not-gate a iwire-a)
    (not-gate b iwire-b)
    (not-gate c iwire-c)
    (tree-wire-and-gate iwire-a
			iwire-b
			iwire-c
			(get-wire 0 out))

    (tree-wire-and-gate iwire-a
			iwire-b
			c
			(get-wire 1 out))

    (tree-wire-and-gate iwire-a
			b
			iwire-c
			(get-wire 2 out))

    (tree-wire-and-gate iwire-a
			b
			c
			(get-wire 3 out))

    (tree-wire-and-gate a
			iwire-b
			iwire-c
			(get-wire 4 out))

    (tree-wire-and-gate a
			iwire-b
			c
			(get-wire 5 out))

    (tree-wire-and-gate a
			b
			iwire-c
			(get-wire 6 out))

    (tree-wire-and-gate a
			b
			c
			(get-wire 7 out))))





