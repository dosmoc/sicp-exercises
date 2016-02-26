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
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;P1 and P2 execute one after the other
;the easy ones:
(set! x (* 10 10))
;x is 100
(set! x (* 100 100 100))
;x is 1000000

;P2 executes after P1
(set! x (* 10 10 10))
;x is 1000
(set! x (* 1000 1000))
;x is 1000000

;P1 reads x of 10, but P2 reads and sets in between
:(set! x (* 10 1000))
;x is 10000

;P2 reads x of 10, but P1 reads and sets in between subsequent reads:
(set! x (* 10 100 100))
;x is 100000

;P2 reads x twice, but P1 reads and sets in between subsequent reads:
(set! x (* 10 10 100))
;x is 10000

;P2 and P1 read x as 10 each, and and each set! occurs at a seperate time:
;x is 100 
;x is 1000

;5 possible values

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;Each procedure is completely serialized; only the first two
;orderings are possible, so x can only be 1000000

;Exercise 3.41 We are asked if the commented change prevents some
;anomalous behavior:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ; serialized
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;Previously, withdraw and deposit were only serialized in the same set,
;preventing concurrent withdraws or deposits to the account. The change
;also prevents balance from being accessed concurrently.

;My first instinct is yes, if a balance is being read at the same time it
;is being changed by another process, a possiblity described by footnote 36.
;If the balance is read in the middle of a change, the data there may 
;consist partly of the old value and partly of the new value.
;The same note says that most computers have interlocks on memory-write 
;operations; it would depend on the implementation of writing values.

;Exercise 3.42 Is it safe to serialize outside of the dispatch procedure?

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

;We get in this version, (protected-withdraw0) and (protected-deposit0) each time 
;whereas in the previous version we might get 
;(protected-withdraw0), (protected-deposit0), (protected-withdraw1), (protected-deposit1),
;(protected-withdraw2), (protected-withdraw3), (protected-deposit2)

;The text of SICP says that 

;"serialization creates distinguished sets
; of procedures such that only one execution of a procedure in each serialized set
; is permitted to happen at a time. If some procedure in the set is being executed, 
; then a process that attempts to execute any procedure in the set will be forced to 
; wait until the first execution has finished."

; So it doesn't matter which particular procedure is serialized, it cannot be executed 
; concurrently with other procedures serialized by that serializer. The behavior of 
; the two versions are the same.

;Complexity of using multiple shared resources

;swapping balances
;access balance
;compute difference
;withdraw diff from one, deposit it the other

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;exposing serializers:

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;must explicitly manage serialization
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

;implementation using accessible serializers
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;Exercise 3.43
;Todo draw diagrams

;Exercise 3.44

;Bitdiddle claims this procedure is sufficient
;for multiple concurrent transfers between
;multiple accounts as long as deposit and withdrawal
;transactions are serialized
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;Louis thinks that a solution is needed similar to that
;of the exchange problem

;Exercise 3.45
;What's wrong with making an account object serialize
;withdraw and deposit
;while at the same time exposing the serializer
;for us in something like serialized-exchange
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (deposit account amount)
 ((account 'deposit) amount))