;2.5 Systems with Generic Operations

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

;From 2.4.2  
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

;Previous packages

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


;2.5.1 Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)) 
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)       
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  ;For exercise 2.81
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
 
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

;make ordinary numbers
(define (make-scheme-number n)
 ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal proceduresnumer
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;added for Exercise 2.79
  (define (equal-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  ;added for Exercise 2.79
  (put '= '(rational) equal-rat?)
  
  ;added for Exercise 2.80
  (put 'denom '(rational) denom)
  (put 'numer '(rational) numer)

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))



(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
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
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
  
;Exercise 2.77
(define z (make-complex-from-real-imag 3 4))

(magnitude z)

;z is this: (complex rectangular 3 . 4)

(define (magnitude z) (apply-generic 'magnitude z))
;apply-generic called ... the argument is (complex)
;retrieves the magnitude procedure again
;this time argument is (rectangular 3 . 4)

(get 'magnitude '(rectangular))

;apply-generic is called twice

;Exercise 2.78
(define (attach-tag type-tag contents) 
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))
(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
          (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum) 
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
          (error "Bad tagged datum -- CONTENTS" datum))))

;Exercise 2.79

(define (install-equ-package)
 
 ;import these parts
 (define (magnitude n)
   ((get 'magnitude '(polar)) n)) 
 (define (angle n)
   ((get 'angle '(polar)) n))
 (define (real-part n)
   ((get 'real-part '(polar)) n))
 (define (imag-part n)
   ((get 'imag-part '(polar)) n))
 
 (define (equ? n m)
   (apply-generic 'equ? n m))
 
 (define (equ-polar? n m) 
   (and (= (magnitude n) (magnitude m))
        (= (angle n) (angle m))))
 
 (define (equ-rectangular? n m) 
   (and (= (real-part n) (real-part m))
        (= (imag-part n) (imag-part m))))
 
 (put 'equ? '(scheme-number scheme-number) =)
 (put 'equ? '(rational rational) (get '= 'rational))
 (put 'equ? '(polar polar) equ-polar?)
 (put 'equ? '(rectangular rectangular) equ-rectangular?)
 ;will this work?
 (put 'equ? '(complex complex) equ?)
 'done)

(install-equ-package)

(define (equ? n m)
  (apply-generic 'equ? n m))

;there's a problem with using equal? in the rational equality check
;in the (equal? 0 0.0) -> #f
;so you could use a similar technique as in the complex number
;equality check...
;however, in the text, the rational selectors where not exported
;I've inserted them into the rational package and updated equ?
;but the decision about which package to include the operator
;definition is still up for grabs


;Exercise 2.80
(define (install-zero?-package)
 (define (numer x)
   ((get 'numer 'rational) x))
  
 (put '=zero? '(scheme-number)
      (lambda (x) (= x 0)))
 
 (put '=zero? '(rational) 
      (lambda (x) (= (numer x) 0)))
 
 (put '=zero? '(polar) 
      (lambda (x) (equ? (make-from-mag-ang 0 0) x)))
 
 (put '=zero? '(rectangular) 
      (lambda (x) (equ? (make-from-real-imag 0 0) x)))
 
 ;this will only work if the complex number is in rectangular form
 (put '=zero? '(complex) 
      (lambda (x) (equ? x (make-complex-from-real-imag 0 0))))
 
 'done)
 
(install-zero?-package)
 
(define (=zero? n)
  (apply-generic '=zero? n))

;for some reason (=zero? (make-rational 0 1)) is not working. Getting this error:
;No method for these types -- APPLY-GENERIC (equ? (rational 0))

;when this works: (equ? (make-rational 0 1) (make-rational 0 1))

;This is because the tag is stripped off by apply-generic
;and needs to be re-tagged
;hmmm ... might be worth it to export denom and numer for use in the equ? and =zero? packages
;so this isn't necessay

;done!

;2.5.2 Combining Data of Different types

(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(add 1 (make-complex-from-real-imag 1 2))

;Exercise 2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (expo x y) (apply-generic 'exp x y))

;a.
;(expo (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
;this appears to not terminate

;The cond where it coerces calls apply-generic
;again with the argument coerced to itself; the recursive
;call still finds no matching procedure for the arguments
;and continues to call itself

;b.
(mul (make-rational 1 2) (make-rational 3 4))
;works as is; Louis is right in that the coercion may happen
;even if they have the same type, but this is only because
;there is no operation defined for that combination of
;types

;c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
           (if (and (= (length args) 2)
                    (not (equal? type1 type2)))
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))))

(expo (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
;works! in that it raises an error if there is no method for those types

;Exercise 2.82
;from exercise 2.3
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (list->set a-list)
  (accumulate (lambda (x set) (adjoin-set x set)) '() a-list))

(define (all-true? bool-list)
  (accumulate (lambda (a b) (and a b)) true bool-list))

(define (all-coerced? val-list)
  (all-true? val-list))

(define (identity x) x)

(define (coerce-to type)
  (lambda (arg)
    (let ((arg-type (type-tag arg))) 
     (let ((proc (get-coercion arg-type type))) 
      (cond ((equal? arg-type type) arg)
            (proc (proc arg))
            (else #f))))))

;redo
(define (apply-generic op . args)
  (define (all-same? items)
    (all-true? (map (lambda (x) (equal? x (car items))) (cdr items))))
  
  (define (coerce-args types)
    (if (not (null? types))
        (let ((current-type (car types)))
          (let ((coerced (map (coerce-to current-type) args)))
            (if (all-coerced? coerced)
	            coerced
	            (coerce-args (cdr types)))))
        (error "No method for these types" )))
  
  (let ((type-tags (map type-tag args)))
   (let ((proc (get op type-tags)))
     (if proc
         (apply proc (map contents args))
         (if (not (all-same? type-tags))
             (apply apply-generic 
                    (cons op (coerce-args (list->set type-tags))))
          	 (error "No method for these types"))))))

;lets add a coercion to test
(mul (make-complex-from-real-imag 1 2) (make-rational 1 2))
;raises the correct error!
(expo (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
;raises the correct error!
(mul (make-complex-from-real-imag 1 2) 1)
;This won't work simply on mixed-type operations that aren't 
;retrieved by (get op (type-tags)) where types are not in the
;same order as in the operation table. 

;I want to do a version that doesn't get the coercion and coerce at
;the same time

;changing the behavior of appy-generic then simply becomes a question
;of defining how to coerce all arguments, and a test to consider 
;it successful
(define (all-same? items)
    (all-true? (map (lambda (x) (equal? x (car items))) (cdr items))))

(define (apply-generic op . args)
   
  (define (get-coercion-to type)
    (lambda (arg)
      (get-coercion (type-tag arg) type)))
  
  (define (found-all-coercions? fns)
    (all-true? fns))
  
  (define (coerce-all fns args)
    (if (null? fns)
        '()
        (let ((fn (car fns))
              (arg (car args)))
          (cons (fn arg) (coerce-all (cdr fns) (cdr args))))))
  
  (define (coerce-args types)
    (if (not (null? types))
        (let ((current-type (car types)))
          (let ((coercion-fns (map (get-coercion-to current-type) args)))
            (if (found-all-coercions? coercion-fns)
	            (coerce-all coercion-fns args)
	            (coerce-args (cdr types)))))
        (error "No method for these types" )))
  
  (let ((type-tags (map type-tag args)))
   (let ((proc (get op type-tags)))
     (if proc
         (apply proc (map contents args))
         (if (not (all-same? type-tags))
             (apply apply-generic 
                    (cons op (coerce-args (list->set type-tags))))
          	 (error "No method for these types"))))))

(mul (make-complex-from-real-imag 1 2) 1)
;works!


;Exercise 2.83
; Unless I'm horribly mistaken, we don't have a package for reals yet
; This is kind of what bothers me, though. What I've read 
; about reals is that the representation from rational -> real
; shouldn't necessarily change. Some SICP answers seem to
; make real numbers as a conversion from exact->inexact
; but for now, I'm just considering a real number as any
; exact or inexact number tagged with 'real
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x)) 
  
  (put 'make 'real
       (lambda (x) (tag x)))
  
  ;the real part is just the contents,
  ;we need this to raise real numbers
  (put 'real-part '(real) contents)
  
  'done)


(define (make-real n)
  ((get 'make 'real) n))


(install-real-package)

(define (install-raise-operations)
  (define (numer n)
    ((get 'numer '(rational)) n))
  
  (define (denom n)
    ((get 'denom '(rational)) n))
  
  (define (real-part n)
    ((get 'real-part '(real)) n))
  
  (define (make-complex-from-real-imag x y)
  	((get 'make-from-real-imag 'complex) x y))
  
  (put 'raise '(scheme-number)
       (lambda (n) (make-rational n 1)))
  (put 'raise '(rational)
       (lambda (n) (make-real (/ (numer n) (denom n)))))
  (put 'raise '(real)
       (lambda (n) (make-complex-from-real-imag (real-part n) 0)))
  'done)

(install-raise-operations)

(define (raise n)
  (apply-generic 'raise n))

(define (denom n)
  (apply-generic 'denom n))

(define (numer n)
  (apply-generic 'numer n))

(get 'denom '(rational))
(get 'numer '(rational))

(raise (raise (make-rational 1 2)))

(type-tag (raise 1))
;rational

(type-tag (raise (raise 1)))
;scheme-number
;crap!
;because the contents of make-real
;is a number, the type-tag procedure
;considers this a scheme-number
(define (attach-tag type-tag contents) 
  (cond ((and (not (equal? type-tag 'real))
              (number? contents)) contents)
        (else (cons type-tag contents))))

(raise 1)
;(rational 1 . 1)
(raise (raise 1))
;(real . 1)
(raise (raise (raise 1)))
;(complex rectangular 1 . 0) allright!

;2.84
(define lowest-type 1)
(define lowest-type-tag (type-tag lowest-type))

(define (add-higher-type n types)
  (let ((raise-fn (get 'raise (list (car types)))))
    (if raise-fn
        (let ((raised (raise-fn (contents n))))
          (add-higher-type raised (cons (type-tag raised) types)))
        types)))

(define (get-higher-types n)
  (add-higher-type n (list (type-tag n))))

;doing it this way means that if new types are added to
;the tower, they will always be included in the tower;
;however, this means that the tower is recalculated each time
;a value needs to be raised
(define (type-tower-list) (get-higher-types lowest-type))

;defining it this way:
;(define type-tower (get-higher-types 1))
;(define (type-tower-list) type-tower)
;means that the type tower is only calculated once, but means
;that dynamically adding types to the tower is not possible
;with re-definining type-tower
;I'm not sure which is better, but abstraction of the 
;type tower as a function allows for the decision to be
;made without affecting the higher-type-of function
(define (inc x) (+ x 1))

(define (index-of s alist)
  (define (count-cars count current-list)
    (cond ((null? current-list) (error "Item not found"))
          ((equal? (car current-list) s) count)
          (else (count-cars (inc count) (cdr current-list)))))
  (count-cars 0 alist))

(define (higher-type-of type1 type2)
  (cond ((< (index-of type1 (type-tower-list))
            (index-of type2 (type-tower-list))) type1)
	    ((> (index-of type1 (type-tower-list))
	        (index-of type2 (type-tower-list))) type2)
        (else type1)))

(define (highest-type-in-list types)
  (accumulate (lambda (type current-type) (higher-type-of type current-type)) 
              lowest-type-tag
              types))

(define (successive-raise n type)
  (if (equal? (type-tag n) type)
      n
      (successive-raise (raise n) type)))

(define (raise-all args type)
  (map (lambda (arg) (successive-raise arg type)) args))

(successive-raise 1 'complex)

(successive-raise (make-complex-from-real-imag 1 2) 'complex)

(define (apply-generic op . args)
  (define (coerce-args types)
    (raise-all args (highest-type-in-list types)))
  
  (let ((type-tags (map type-tag args)))
   (let ((proc (get op type-tags)))
     (if proc
         (apply proc (map contents args))
         (if (not (all-same? type-tags))
             (apply apply-generic 
                    (cons op (coerce-args (list->set type-tags))))
          	 (error "No method for these types"))))))

(mul 4 (make-rational 1 2))
;(rational 2 . 1)
;works!

(mul 4 (make-complex-from-real-imag 1 3))
;also works!

;Exercise 2.85
(define (project n)
  (apply-generic 'project n))

;I realize it makes more sense to put these functions
;in their relative type packages, but putting them all 
;in one place for convenience right now
(define (install-project-operations)
  (define (numer n)
    ((get 'numer '(rational)) n))
  
  (define (denom n)
    ((get 'denom '(rational)) n))
  
  (put 'project '(rational)
       (lambda (n) (make-scheme-number (/ (numer n) (denom n)))))
  (put 'project '(real)
       (lambda (n) (make-rational (real-part n) 1)))
  (put 'project '(complex)
       (lambda (n) (make-real (real-part n))))

  'done)


(define (drop n)
  (let ((projected (project n)))
    (if (equ? n (raise projected))
	    (drop projected)
	    n)))

(define (apply-generic op . args)
  (define (coerce-args types)
    (if (not (null? types))
        (let ((highest-type (highest-type-in-list types)))
          (let ((raised (map (lambda (arg) (successive-raise arg highest-type)) args)))
            (apply (apply-generic (cons op coerced)))))
        (error "No method for these types")))

  (drop
    (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (apply apply-generic 
                  (cons op (coerce-args (list->set type-tags)))))))))
