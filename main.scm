;; Little magics:
;; from timeless and infinite structures to beyond
;; EXPLORE STRUCTURAL MAGIC

;; define delay as special form called promise
;; as (memoize (lambda () <exp>))
(define (memoize f)
  (let ((run-once #f)
	(result '()))
    (lambda ()
      (if run-once
	  result
	  (begin
	    (set! run-once #t)
	    (set! result (f))
	    result)))))

;; my own version of delay
(define-syntax promise
  (syntax-rules ()
    ((promise exp)
     (memoize
      (lambda ()
	exp)))))

(define (force-promise p)
  (p))

(define-syntax stream
  (syntax-rules ()
    ((stream n exp)
     (cons n (promise exp)))))

(define (generate-stream init generator)
  (stream init
	  (generate-stream
	   (generator init)
	   generator)))

(define (stream-cdr s) (force-promise (cdr s)))

(define (take n s)
n  (cond ((null? s) '())
	((= 0 n) '())
	(else (cons (car s)
		    (take (- n 1)
			  (stream-cdr s))))))

(define empty-stream '())
(define stream-null? null?)

(define (map-s fun s)
  (if (stream-null? s) empty-stream
      (stream (fun (car s))
	      (map-s fun (stream-cdr s)))))

;;map-s with a list of streams as args
(define (map-sn proc . streams)
  (if (null? streams) empty-stream
      (stream
       (apply proc (map car streams))
       (apply map-sn (cons proc (map stream-cdr streams))))))

;;test
(define test
  (take 20
	(map-sn + (generate-stream 1
				   (lambda (n) (+ 1 n)))
		(generate-stream 2
				 (lambda (n) (+ 2 n))))))

(define (show x)
  (print x)
  x)

(define (stream-ref n s)
  "stream-ref take the nth element from a stream"
  (cond ((= 0 n) (car s))
	((stream-null? s) empty-stream)
	(else
	 (stream-ref (- n 1) (stream-cdr s)))))

(define (enumerate n l)
  (if (> n l) empty-stream
      (stream n
	      (enumerate (+ n 1) l))))

(define *t* (map-sn show (enumerate 1 10000)))

(define (filter pred s)
  (cond ((stream-null? s) empty-stream)
	((pred (car s))
	 (stream (car s)
		 (filter pred (stream-cdr s))))
	(else (filter pred (stream-cdr s)))))


;; test #2
(define sum 0)
(define (acc n)
  (set! sum (+ sum n))
  sum)
(define seq (map-sn acc (enumerate 1 500)))
(define y (filter even? seq))
(define z (filter (lambda (x) (= (remainder x 5) 0)) seq))

;; how define the sieve of Eratosthenes with infinite streams
(define (divisible? x y)
  (= 0 (remainder x y)))

(define (sieve s)
  (stream
   (car s)
   (sieve
    (filter
     (lambda (x) (not (divisible? x (car s))))
     (stream-cdr s)))))

(define primes (sieve (generate-stream 2 (lambda (x) (+ 1 x)))))

;; One has to think about this definition in terms of TAKE function:
;; fibs doesm't change (it is a variable, so can change only with SET!),
;; but the parameter for TAKE does, as it is a recursive procedure.
;; So in every recursive step, the input stream is the result of forcing
;; the delayed part of the stream itself.
;; Streams are all about preloading the next value to produce, but one
;; needs means to extract those values from them.

(define fibs
  (stream 0
	  (stream 1
		  (map-sn + (stream-cdr fibs)
			  fibs))))

(define (mult-streams . streams)
  (apply map-sn (cons * streams)))

(define (inc n) (+ 1 n))
(define integers (generate-stream 1 inc))

(define factorial
  (stream 1
	  (mult-streams integers
		        factorial)))
;; mutable structure study
;; a data structure has a constructor and selectors.
;; Defined in this way the data structure is immutable
;; because there are only means to access data and
;; not to mutate them.
;; A mutable data structure has mutators.

;; remember that the return value of set! is implementation
;; dependant, so it has to be used only for its effect
;; and not for its return value.

;; in chicken scheme set-car! is a primitive.
;; in order to redefine procedures, i put m
;; before the name.

(define (mset-car! x y)
  (set! (car x) y))

;; imagine define as a syntactic sugar for a
;; global let (even though im not sure of that),
;; so x is the reference copy of the real input
;; (the copy of the pointer), same for y.
;; so set! over x or y is basically useless,
;; we need to access the car or cdr real value (always pointers)
;; through the copy pointer in order to use set!
;; expecting some result.

(define (mset-cdr! x y)
  (set! (cdr x) y))

(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (mappend! x y)
  (mset-cdr! (last-pair x) y))

;; if instead of 'queue-t we put 'queue
;; something strange happens -> can you find why? 

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


;; deque -> it is bad written, but it was useful
;; to understand how conses actually work with
;; mutable operations.

;; Can be totally rewritten
;; for istance, pop/push can be defined as
;; function interfaces that accept a symbol
;; between 'head or 'tail and dispatch the
;; functionality.
;; Another improvement could be to define
;; specific access functions instead of
;; cdar/cdadr/etc family.
;; Moreover, there is too much branching.

(define (deque)
  (let ((P '()))
    (lambda (sym)
      (define (push-t! val)
	(let ((new (list (list val))))
	  (if (null? P)
	      (begin
		(set! P (list new))
		(set-cdr! P (car P)))
	      (let ((tmp (cdr P)))
		(set-cdr! (car new) (delay tmp))
		(set-cdr! (cdr P) new)
		(set-cdr! P (cddr P))
		))
	  P))
      
      (define (get-promise-t) (cdadr P))
      (define (get-first) (caar P))
      
      (define (pop-t!)
	(if (null? (cdar P))
	    (begin 
	      (set! P '())
	      P)
	    (let ((new-last (force (get-promise-t))))
	      (set-cdr! new-last '())
	      (set-cdr! P new-last)
	      P)))

      (define (push-h! val)
	(let ((new (list (list val))))
	  (if (null? P)
	      (begin
		(set! P (list new))
		(set-cdr! P (car P)))
	      (let ((first (get-first)))
		(set-cdr! new (car P))
		(set-car! P new)
		(let ((tmp (car P)))
		  (set-cdr! first (delay tmp)))))
	  P))
      
      (define (pop-h!)
	(if (null? (cdar P))
	    (begin 
	      (set! P '())
	      P)
	    (let ((second (cadar P)))
	      (set-car! P (cdar P))
	      (set-cdr! second '())
	      P)))
      
      (define (to-list) (map car (car P)))

      (cond
       ((eq? sym 'push-t!) push-t!)
       ((eq? sym 'pop-t!) pop-t!)
       ((eq? sym 'to-list) to-list)
       ((eq? sym 'push-h!) push-h!)
       ((eq? sym 'pop-h!) pop-h!)
       (else (error 'function-not-found)))
      )))

(define (push-t! dq val) ((dq 'push-t!) val))
(define (pop-t! dq) ((dq 'pop-t!)))
(define (to-list dq) ((dq 'to-list)))
(define (push-h! dq val) ((dq 'push-h!) val))
(define (pop-h! dq) ((dq 'pop-h!)))


;; Binary Tree:

;; so the structure will be something like this:
;; - for a single node (val (L) (R))
;; - where both L and R are node
;; so to represent a tree like:

;; *tree* -->  6
;;            / \
;;           4   7
;;          / \
;;         3   5

;; will be '(*tree* 6 (4 (3) (5)) (7)).

;; In order to make it mutable, we initialize
;; the tree like a simple header list (list '*tree*)
;; so we can use set-cdr! on it to insert! the
;; first value.

;; for now lets pretend that tree will always
;; contains the header *tree*.
;; Moreover, we will just insert numbers for now.

;; structure of nodes:

;; (*tree* _)--> (val _)
;;                    |___:> (_ _)
;;                            | |___:> (Rval (left right))
;;                            |
;;                            |__:> (Lval (left right))

;; there are basically 3 cases
;; if v is > than the value in the node
;; we put it at its right if there is no
;; right already. Otherwise we call put-node!
;; recursively with the right node and
;; the value v.
;; Same for the left node.

(define (make-node val) (cons val (cons '*L* '*R*)))
(define (left-node node) (cadr node))
(define (right-node node) (cddr node))
(define (node-val node) (car node))

(define (insert! node val)
  (define (lookup sym setter! getter)
          (cond ((eq? sym (getter node))
	     (setter! (cdr node) (make-node val)))
	    (else (insert! (getter node) val))))

  (if (> val (node-val node))
      (lookup '*R* set-cdr! right-node)
      (lookup '*L* set-car! left-node)))

(define (tree->list node)
  (if (atom? node)
      '()
      (let ((l (tree->list (left-node node)))
	    (r (tree->list (right-node node)))
	    (v (node-val node)))
	(append l (list v) r))))


;; This is a horrible definition of make-tree
;; I was to lazy to redefine better the function
;; that I've already created.
;; Can be designed better than this, but this definition
;; permits to pass an infinite number of arguments
;; plus a compare function to sort the tree in a custom
;; way.

;; Example:

;; (define tree (make-tree < 2 6 3 1 9 8))
;; ((tree 'tree->list))
;; => (9 8 6 3 2 1)

;; I've used header parameters like *L* and *R* as
;; placeholders to make easier to insert new values
;; inside the tree. However, probably there is a
;; better solution.
  
(define (make-tree cmp . args)
  (let ((tree '())
	(fcmp cmp))
    
    (lambda (sym)
      (define (make-node val) (cons val (cons '*L* '*R*)))
      (define (left-node node) (cadr node))
      (define (right-node node) (cddr node))
      (define (node-val node) (car node))

      (define (tree->list)
	(define (step node)
	  (if (or (atom? node)
		  (null? node))
	      '()
	      (let ((l (step (left-node node)))
		    (r (step (right-node node)))
		    (v (node-val node)))
		(append l (list v) r))))
	(step tree))

      (define (insert-in-tree! val)
	(define (insert! node val)
	  (define (lookup sym setter! getter)
	    (cond ((eq? sym (getter node))
		   (setter! (cdr node) (make-node val)))
		  (else (insert! (getter node) val))))

	  (if (fcmp val (node-val node))
	      (lookup '*R* set-cdr! right-node)
	      (lookup '*L* set-car! left-node)))
	
	(if (null? tree)
	    (set! tree (make-node val))
	    (insert! tree val)))

      (define (push-all! vals)
	(if (null? vals) 'done
	    (let ((first (car vals)))
	      (insert-in-tree! first)
	      (push-all! (cdr vals)))))

      (unless (null? args) (push-all! args))

      (cond
       ((eq? sym 'insert!) insert-in-tree!)
       ((eq? sym 'tree->list) tree->list)
       (else 'method-not-found))
      )))
		 
;; So now is the time for tables and tabulation (memoization using
;; tables to save the result in a (parameter result) pair).

;; I have to design a bidimensional table, then a table which
;; takes an infinite number of keys to look for and a sorted
;; table, which uses a binary tree way to insert and lookup
;; values. At the end we can create a tabulation function
;; using tables to memoize a function result based on its params.

;; Simple definition for a one dimension table
;; 'assoc' takes O(n) to check if a key is present or not.
;; In order to make it O(log(n)) we can implement a
;; binary tree search, setting a cmp function when
;; calling make-table (eg.: (make-table <))
;; The put! function makes use of assoc function and
;; if no key is found, insert the new pair at the beginning
;; of the tree, so it takes O(n).

(define (make-table)
  (let ((t '()))
    (lambda (sym)

      (define (assoc key)
	(define (assoc-step key table)
	  (cond
	   ((null? table) #f)
	   ((equal? key (caar table)) (car table))
	   (else (assoc-step key (cdr table)))))
	(assoc-step key t))


      (define (put! key val)
	(let ((prev (assoc key)))
	  (if prev
	      (set-cdr! prev val)
	      (set! t
		    (cons (cons key val) t)))
	  t))

      (cond
       ((eq? sym 'put!) put!)
       ((eq? sym 'assoc) assoc)
       (else (error 'table-method-not-found))))))

(define (put! table key val) ((table 'put!) key val))
(define (assoc table key) ((table 'assoc) key))


;; tabulation
(define (memoize-tb fun)
  (let ((table (make-table)))
    (lambda args
      (let ((pair (assoc table args)))
	(if pair (cdr pair)
	    (let ((result (apply fun args)))
	      (put! table args result)
	      result))))))

;;Multi-Dimensional Table

;; I don't want to introduce exception handling for now,
;; but I'm wondering if there is a nice way to check types
;; for constructors that return a procedure.

(define (tablep obj)
  (and (procedure? obj)
       (eq? '_sym_table
	    (cadr (procedure-information obj))
	    )))

;; It DOES work, but its a lot of code and I'm sure
;; there is a better way to design this structure.

(define (make-table cmp)
  (let ((table (list '*table*))
	(cmp cmp))
    (lambda (_sym_table)
      
      (define (assoc . keys)
	(define (assoc-step node . keys)
	  (cond
	   ((or
	     (atom? node)
	     (null? node)
	     (null? keys)) #f)
	   ((cmp (car keys)
		 (get-key node))
	    (apply assoc-step (cons (get-right node)
				    keys)))
	   ((eq? (car keys)
		 (get-key node))
	    (cond
	     ((null? (cdr keys)) (get-val node))
	     ((tablep (get-val node))
	      (apply ((get-val node) 'assoc) (cdr keys)))
	     (else #f)))
	   (else
	    (apply assoc-step (cons (get-left node)
				    keys)))))
	(apply assoc-step (cons (cdr table) keys)))
      
      (define (get-val node) (cadr node))
      (define (get-children node) (cddr node))
      (define (get-right node) (cdr (get-children node)))
      (define (get-left node) (car (get-children node)))
      (define (get-key node) (car node))

      (define (set-val! node val) (set-car! (cdr node) val))
      (define (set-right! node val)
	(set-cdr!(get-children node) val))
      (define (set-left! node val)
	(set-car! (get-children node) val))

      (define (make-node val key . keys)
	(if (null? keys)
	    (append (list key val) (cons 'L 'R))
	    (append (list key
			  (let ((new-table (make-table cmp)))
			    (apply (new-table 'insert!)
				   (cons val keys))
			    new-table))
		    (cons 'L 'R))))

      (define (create-child-inserter sym)
	(let ((getter (if (eq? 'R sym)
			  get-right
			  get-left))
	      (setter! (if (eq? 'R sym)
			   set-right!
			   set-left!)))
	  (lambda (node val key . keys)
	    (if (eq? sym (getter node))
		(setter! node (apply make-node
				     (append (list val key) keys)))
		(apply dispatch-insert!
		       (append
			(list (getter node) val key)
			keys))))))
      (define right-inserter (create-child-inserter 'R))
      (define left-inserter (create-child-inserter 'L))
      (define (insert-right! node val key . keys)
	(apply right-inserter
	       (append (list node val key) keys)))
      (define (insert-left! node val key . keys)
	(apply left-inserter
	       (append (list node val key) keys)))
      (define (insert-val! node val key . keys)
	(let ((nval (get-val node)))
	  (cond
	   ((null? keys)
	    (set-val! node val))
	   ((tablep nval)
	    (apply (nval 'insert!) (cons val keys)))
	   (else
	    (set-val! node
		      (let ((new-table (make-table)))
			(apply (new-table 'insert)
			       (cons val keys))
			new-table))))))

      (define (dispatch-insert! node val key . keys)
	(let ((nkey (get-key node)))
	  (cond
	   ((eq? nkey key)
	    (apply insert-val! (append (list node val key) keys)))
	   ((cmp key nkey)
	    (apply insert-right! (append (list node val key) keys)))
	   (else
	    (apply insert-left! (append (list node val key) keys))))))

      (define (insert! val key . keys)
	(if (null? (cdr table))
	    (set-cdr! table (apply make-node (append (list val key) keys)))
	    (apply dispatch-insert!
		   (append (list (cdr table) val key) keys)))
	table)

      (define (to->list)
	(define (step->l node)
	  (if 
	   (or (atom? node)
	       (null? node)) '()
	       (let ((left (step->l (get-left node)))
		     (right (step->l (get-right node))))

		 (if (tablep (get-val node))
		     (append
		      (list (get-key node)
		       (((get-val node) 'to->list)))
		      left
		      right)
		     (append (list (get-key node)
				   (get-val node))
			     left
			     right)))))
	(step->l (cdr table)))
	

      (cond
       ((eq? _sym_table 'insert!) insert!)
       ((eq? _sym_table 'assoc) assoc)
       ((eq? _sym_table 'to->list) to->list)
       (else (error 'definition-not-found)))
      )))

