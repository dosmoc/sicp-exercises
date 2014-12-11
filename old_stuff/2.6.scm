(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))

;evaluate (add-1 zero)

;body of add-1
;(lambda (f) (lambda (x) (f ((n f) x))))

;substitute zero
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

;reduce
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

;evaluate operator:
;(lambda (f) (lambda (x) (f (((lambda (x) x) f) x))))

;apply:
;(lambda (f) (lambda (x) (f (f x))))

;so one can be defined directly as:
(define one (lambda (f) (lambda (x) (f x))))

;lets try two!
;evaluate (add-1 one)

;body of add-1
;(lambda (f) (lambda (x) (f ((n f) x))))

;substitute one
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (x)))) f) x))))

;;pull this one out 'cause my brain:
;((lambda (f) (lambda (x) (f (x)))) f)

;;substitute:
;(lambda (x) (f x))

;so the reduced form:
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))

;so this expression:
;((lambda (x) (f x)) x)

;reduces to:
;(f x)

;so that reduction is:
;(lambda (f) (lambda (x) (f (f x))))

;so two is:
(define two (lambda (f) (lambda (x) (f (f x)))))

;defining add

;well, intuition is: get the results of one and then apply the other to it
;what about applying an number to the other:
(define (add n m)
	(lambda (f) (lambda (x) (m ((n f) x)))))


;for example
(add two one)

;nope, since the one function is substituted for f
;we want it to be (f (f (f x))) 
;(lambda (f) (lambda (x) ((f x) ((f x) x))))

;(define (add n m)
;  (let ((n-x (n x)))
;    (m x)))

;the solution is--didn't figure it out myself, but seems obvious in retrospect:
(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))

;this is bending my brain

;multiplication?
(define (mul n m)
	(lambda (f) (lambda (x) ((m (n f)) x))))


(define four (mul two two))

((four (lambda (x) (display x) (newline) x)) "hello")