;3.4  Concurrency: Time Is of the Essence


;3.4.1  The Nature of Time in Concurrent Systems

;Exercise 3.38

;a.
;Balance in intially $100
;Peter:	(set! balance (+ balance 10))
;Paul:	(set! balance (- balance 20))
;Mary:	(set! balance (- balance (/ balance 2)))

(set! balance (+ balance 10)) ;110
(set! balance (- balance 20)) ;90
(set! balance (- balance (/ balance 2)))
;balance is $35

(set! balance (- balance 20)) ;80
(set! balance (+ balance 10)) ;90
(set! balance (- balance (/ balance 2))) 
;balance is $35

(set! balance (- balance 20)) ;80
(set! balance (- balance (/ balance 2))) ;40
(set! balance (+ balance 10))
;balance is $50

(set! balance (- balance (/ balance 2))) ;50
(set! balance (- balance 20)) ;30
(set! balance (+ balance 10))
;balance is $40

(set! balance (- balance (/ balance 2))) ;50
(set! balance (+ balance 10)) ;60
(set! balance (- balance 20))
;balance is $40

(set! balance (+ balance 10)) ;110
(set! balance (- balance (/ balance 2))) ;55
(set! balance (- balance 20)) 
;balance is $35

;3 different possible values, $35, $40 and $50

;b.
;| Peter           Paul            Mary
;| access balance  access balance  access balance
;| $100            $100            $100
;|
;| new value:      new value:      new value:
;| $110            $80             $50
;|
;| $110            
;|                 $80
;|                                 $50
;V 
;Balance accessed all at once could potentially
;lead to any one of $110, $80, or $50

;Serializers in scheme.

;MIT Scheme specific implementation of
; parallel-execute from parallel.scm, but it 
;doesn't seem to work
(define disallow-preempt-current-thread
  (access disallow-preempt-current-thread
	  (->environment '(runtime thread))))

(define allow-preempt-current-thread
  (access allow-preempt-current-thread
	  (->environment '(runtime thread))))

(define (kill-thread thread)
  (let ((event
	        (lambda ()
	          (exit-current-thread 'RIP))))
       (without-interrupts
         (lambda ()
           (case (thread-execution-state thread)
	               ((STOPPED) (restart-thread thread #t event))
	               ((DEAD) unspecific)
	               (else (signal-thread-event thread event)))))))

(define (parallel-execute . thunks)
  (let ((my-threads '()))
    (define (terminator)
      (without-interrupts
        (lambda ()
        	 (for-each kill-thread my-threads)
      	   (set! my-threads '())
      	   unspecific)))
    (without-interrupts
      (lambda ()
        (set! my-threads
	      (map 
          (lambda (thunk)
		        (let ((thread (create-thread #f thunk)))
		          (detach-thread thread)
		           thread))
		      thunks))
        unspecific))
    terminator))

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

x

;The MIT-Scheme website proves an implementation of parallel:
;https://mitpress.mit.edu/sicp/psets/ps7/parallel.scm
;
;However, it's difficult to see the thunks in parallel-execute 
;being interleaved-- possibly because they aren't doing enough
;work (and thus take enough time) to be preempted. See discussion
;of threads for Racket here: 
;http://lists.racket-lang.org/users/archive/2006-December/015852.html
; and here:
;http://lists.racket-lang.org/users/archive/2006-January/011112.html
;
;It may be possible to get a finer grained simulation of parallel execution
;using engines: http://lists.racket-lang.org/users/archive//2002-September/000620.html


;possible values for the about paralle-execute:

;101:  P1 sets x to 100 and then P2 increments x to 101.
;121:  P2 increments x to 11 and then P1 sets x to x times x.
;110:  P2 changes x from 10 to 11 between the two times that P1 accesses the value of x during the evaluation of (* x x).
;11:   P2 accesses x, then P1 sets x to 100, then P2 sets x.
;100:  P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.


;Exercise 3.39 
;Which of the five possibilities in the parallel execution shown above
;remain if we instead serialize execution as follows:

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; Accessing x is serialized in P1
; All of P2 is serialized
; 121 cannot occur because (* x x) is serialized
; 11 cannot occur because all of P2 is serialized
; So 101, 110, and 100 are the remaining possibilities

; Exercise 3.40.  
; Give all possible values of x that can result from executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; Which of these possibilities remain if we instead use serialized procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))