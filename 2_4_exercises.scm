
;-----------
;from section 3.3.3 for section 2.4.3
;to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;-----------

;2.4  Multiple Representations for Abstract Data

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) 
  (cons (* r (cos a)) (* r (sin a))))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;2.4.2  Tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))


(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;2.4.3  Data-Directed Programming and Additivity

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;Exercise 2.73

(define (deriv exp var)
	(define (variable? x) (symbol? x))

	(define (same-variable? v1 v2)
	  (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
         (else 
           (display exp)
           ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;testing
(operator '(+ 1 2 3))
(operands '(+ 1 2 3))

;a. Since we consider the type tag to be the first symbol, 
;we use it to get the appropriate procedure from a table
;to derive. Numbers and variables don't have an operand symbol
;associated with them, so there is no tag to dispatch on

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;b
(define (install-deriv-package)
   
	(define (make-sum a1 a2)
	  (cond ((=number? a1 0) a2)
	        ((=number? a2 0) a1)
	        ((and (number? a1) (number? a2)) (+ a1 a2))
	        (else (list '+ a1 a2))))

	(define (make-product m1 m2)
	  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	        ((=number? m1 1) m2)
	        ((=number? m2 1) m1)
	        ((and (number? m1) (number? m2)) (* m1 m2))
	        (else (list '* m1 m2))))
 
	(define (multiplier p) (car p))

	(define (multiplicand p) (cadr p))
 
	(define (addend s) (car s))

	(define (augend s) (cadr s))
 
 	(put 'deriv '+
      (lambda (operands var)
        (make-sum (deriv (addend operands) var)
                  (deriv (augend operands) var))))
        
    (put 'deriv '*
      (lambda (operands var) 
        (make-sum
          (make-product (multiplier operands)
                   	    (deriv (multiplicand operands) var))
          (make-product (deriv (multiplier operands) var)
            	        (multiplicand operands)))))
    

    
    (put 'make 'sum make-sum)
    
    (put 'make 'product make-product)
    )

(install-deriv-package)

(define (make-sum a1 a2)
  ((get 'make 'sum) a1 a2))

(define (make-product m1 m2)
  ((get 'make 'product) m1 m2))

;c
(define (install-deriv-exp-package) 
	(define (make-exponent base exponent) 
	  (cond ((=number? exponent 0) 1)
	        ((=number? exponent 1) base)
	        (else (list '** base exponent))))
    
    (define (base e) (car e))

	(define (exponent e) (cadr e))
 
   	;interface to rest of system
    (put 'deriv '**
      (lambda (operands var)
        (make-product 
          (make-product (exponent operands)
                        (make-exponent (base operands) 
                                       (make-sum (exponent operands) -1)))
          (deriv (base operands) var))))
    
    (put 'make 'exponent make-product)
   )

(install-deriv-exp-package)

(define (make-exponent b e)
  ((get 'make 'exponent) b e))

(get 'deriv (operator '(+ x 3)))

(operator '(+ x 3))
  
(deriv '(+ x 3) 'x)
;1
(deriv '(* x y) 'x)
;y
(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

(deriv '(+ 1 (** x 3)) 'x)

;(* 3 (** x 2))

;d. We would have to change each put into the operations table
;   to specify the operator first and 'deriv second.
;   It's just considering the operation to be sum, product, etc.
;   and the type to be deriv.

;Exercise 2.74

;Todo

;Message passing

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
 		  ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) 
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch) 

(define (apply-generic op arg) (arg op))

;Exercise 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
 		  ((eq? op 'imag-part) (* x (sin y)))
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch) 

;Exercise 2.76

;Best for new types: message-passing style
;It's more obvious to have all the type operations in one
;place
;Best for new operations: data directed style
;Easier to add new operations
;Bleh