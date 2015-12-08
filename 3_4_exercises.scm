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
; parallel-execute from parallel.scm
;;; To allow parallel execution of any number of thunks, for
;;; effect.  The values are discarded.

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
	     (map (lambda (thunk)
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