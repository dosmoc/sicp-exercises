;2.5.3  Example: Symbolic Algebra
;Arithmetic on polynomials

(load "./2_5_exercises.scm")

;a simple polynomial in x:
;5x² + 2x + 7 

;a polynomial in x with coefficients that are polynomials in y:
;(y² + 1)x³ + (2y)x + 1 

;Are (= '(5x² + 2x + 7 ) '(5x² + 2x + 7 ))?
;Yes if we are considering them as mathematical functions,
;no if they are a syntatic form
;
; should the system recognize this?
; how should it be represented?
;
; SICP makes the decision that a polynomial will be
; a particular syntactic form
;
; - just addition and multiplication
; - polynomials to be combined will have the same indeterminate


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))  
  (define (variable? x) (symbol? x))
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; continued on next page
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ;<procedures used by add-poly>
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else 
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  ;<procedures used by mul-poly>
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
    
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  ;From Exercise 2.87
  (put '=zero? '(polynomial) 
       (lambda (p) 
         (= 0 (reduce add 0 (map coeff (term-list p))))))
  
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;for now let's use the basic apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define polynomial-terms-b '((100 1) (2 2) (0 1)))
(define polynomial-terms-c '((100 0) (2 0) (0 0)))
(define polynomial-terms-d '((100 5) (2 3) (0 1)))

(add (make-polynomial 'x polynomial-terms-c) (make-polynomial 'x polynomial-terms-b))
;Value 64: (polynomial x (100 5) (2 7) (0 8))

(mul (make-polynomial 'x polynomial-terms-d) (make-polynomial 'x polynomial-terms-b))
;Value 65: (polynomial x (200 5) (102 13) (100 6) (4 6) (2 5) (0 1))

;Exercise 2.87
;lets define them outside of the package to test

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(adjoin-term (make-term 1 1) '())

(map type-tag '((1 1)))

 (coeff (make-term (make-scheme-number 1) (make-scheme-number 1)))
 
 (type-tag 1)

;=zero? for a polynomial must test to see if
;all coefficients of the polynomial are zero
;assuming we're using the representation above

;If the sum of coefficients is zero, the polynomial
;is zero
(= 0 (reduce add 0 (map coeff polynomial-terms-b)))
;#f

(= 0 (reduce add 0 (map coeff polynomial-c)))
;#t

;see the procedure added to the polynomial package above
(=zero? (make-polynomial 'x polynomial-terms-c))
(=zero? (make-polynomial 'x polynomial-terms-b))

(make-polynomial 'x polynomial-terms-b)
(make-polynomial 'x polynomial-terms-c)
(make-polynomial 'nil polynomial-terms-c)

; We haven't defined equ? to test for =zero?
; We could say that two polynomials are equ? if they
; have the same variable and the same term-list.
; However, equ? would have to adjust the number of terms
; to each other if there are different numbers of terms

; I've just read a little on wikipedia about polynomial
; identity testing... p¹ = p² if p¹ - p² = 0. So you 
; could use polynomial negation + addition to determine
; if a polynomial is equ? and if it's =zero?

;Exercise 2.88

(define (negate n)
  (apply-generic 'mul -1 n))

;Exercise 2.89

;Exercise 2.90
;This one sounds the funnest

;Exercise 2.91
;(define (div-terms L1 L2)
;  (if (empty-termlist? L1)
;      (list (the-empty-termlist) (the-empty-termlist))
;      (let ((t1 (first-term L1))
;            (t2 (first-term L2)))
;        (if (> (order t2) (order t1))
;            (list (the-empty-termlist) L1)
;            (let ((new-c (div (coeff t1) (coeff t2)))
;                  (new-o (- (order t1) (order t2))))
;              (let ((rest-of-result
;                     <compute rest of result recursively>
;                     ))
;                <form complete result>
;                ))))))