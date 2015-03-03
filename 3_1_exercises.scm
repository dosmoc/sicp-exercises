;3.1  Assignment and Local State

;3.1.1  Local State Variables

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 15)
;this makes me nervous

;new forms, set! and begin

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)
(W2 40)
(W1 40)

;down the rabbit hole
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)
;50
((acc 'withdraw) 60)
;insufficient funds
;so in most OO languages, this would look something like:
;acc.withdraw(60);
; where . is member access...
;Clojure / Java interop does (. object methodname args*)
((acc 'deposit) 30)
;should be 80

(define acc2 (make-account 100))
;I've changed my mind, I don't like using mutation in Scheme

;Exercise 3.1
(define (make-accumulator initial)
  (lambda (augend) 
    (set! initial (+ initial augend))
    initial))
;okay, so body of a lambda is like an implicit begin

(define A (make-accumulator 5))

(A 10)
;15
(A 10)
;25

;Exercise 3.2
(define (inc x) (+ x 1))

(define (make-monitored f)
  (let ((calls 0))
    (lambda (m) 
    	(cond ((eq? m 'how-many-calls?) calls)
           	  ((eq? m 'reset-count) 
               	(begin 
                  (set! calls 0)
                  calls))
        	  (else (begin 
            	      (set! calls (inc calls))
                      (f m)))))))

(define s (make-monitored sqrt))

(s 100)
;10
(s 'how-many-calls?)
;1
(s 100)
;10
(s 'how-many-calls?)
;2
(s 'reset-count)
;0 should this even return something?
(s 'how-many-calls?)
;0

;I take it back, 
;(+ mutability dynamic-typing first-class-procedures)
;is pretty awesome

;Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;60
((acc 'some-other-password 'deposit) 50)
;Incorrect password is raised

;Exercise 3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (bad-pw-msg x) "Incorrect password")
  (define (call-the-cops x) "Calling cops")
  
  (define dispatch
    (let ((bad-access 0))
     (lambda (pw m)
       (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (begin
          (set! bad-access (+ bad-access 1))
          (if (> bad-access 7)
              call-the-cops
              bad-pw-msg))))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'withdraw) 50)
;do this 8 times and you get
;"Calling cops"
;I see why first class functions could be considered the
;poor man's objects
;
;That said, I wouldn't want to mangle objects to get
;something close to the usefulness first class functions
;
;I like that assignment feels so awkward in Scheme or Clojure

;3.1.2 The Benefits of Introducing Assignment

(define random-init 5)

;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)   
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

;Exercise 3.5.  Monte Carlo integration
(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-circle-predicate radius center-point)
  (lambda (test-point)
    (>= (square radius)
	    (+ (square (- (x-point test-point)
	                  (x-point center-point)))
	       (square (- (y-point test-point)
	                  (y-point center-point)))))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (test)
    (let ((x (random-in-range x2 x1))
          (y (random-in-range y2 y1)))
      (let ((test-point (make-point x y)))  
        (p test-point))))
  
  (let ((area (* (- x1 x2) (- y1 y2))))
   (* area (monte-carlo trials test))))

(define unit-circle (make-circle-predicate 1 (make-point 0 0)))

(estimate-integral unit-circle 1.0 -1.0 1.0 -1.0 50000)

;Exercise 3.6   
(define rand
  (let ((x random-init))
    (define generate
      (lambda () (set! x (rand-update x)) x))
    (define reset
      (lambda (newval) (set! x newval) x))
    (lambda (m)
      ;if generate is not applied, then a user
      ;would have to write ((rand 'generate))
      ;it's like the difference between writing
      ;rand.generate and rand.generate()
      ;in something like python
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Uknown operation"))))))

;3.1.3 The Costs of Introducing Assignment

;Exercise 3.8
(+ (f 0) (f 1))
;return 0 if arguments are evaluated left to right
;1 if args are evaluated from right to left

;-> we want to get this
(+ 0 0)
;<- we want to get this
(+ 0 1)

(define f
  (let ((x 0))
    (lambda (n)
           (set! x (- n x))
           x)))

(+ (f 0) (f 1))
;mit-scheme goes left to right
